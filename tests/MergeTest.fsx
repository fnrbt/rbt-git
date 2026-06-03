// F2 conformance: 3-way merge vs canonical git. The merged tree is content-
// addressed, so a clean merge must produce the SAME tree OID as
// `git merge-tree --write-tree`. Conflicts must be detected.
//   dotnet build src/FSharpGit/FSharpGit.fsproj -c Debug && dotnet fsi tests/MergeTest.fsx
#I __SOURCE_DIRECTORY__
#r "../src/FSharpGit/bin/Debug/net10.0/FSharpGit.dll"

open System
open System.IO
open System.Diagnostics
open FSharpGit

let mutable failures = 0
let check name cond = if cond then printfn "  ok   - %s" name else (failures <- failures + 1; printfn "  FAIL - %s" name)

let run (wd: string) (cmd: string) (args: string list) : int * string =
    let psi = ProcessStartInfo(cmd, RedirectStandardOutput = true, RedirectStandardError = true, UseShellExecute = false, WorkingDirectory = wd)
    for a in args do psi.ArgumentList.Add a
    use p = Process.Start psi
    let o = p.StandardOutput.ReadToEnd()
    let e = p.StandardError.ReadToEnd()
    p.WaitForExit()
    p.ExitCode, (o + e)
let git wd args = run wd "git" args
let revParse wd r = (snd (git wd ["rev-parse"; r])).Trim()

let tmp = Path.Combine(Path.GetTempPath(), "fsgit-merge-" + Guid.NewGuid().ToString("N").Substring(0, 8))
Directory.CreateDirectory tmp |> ignore
let sig0 : Signature = { Name = "Forge"; Email = "forge@redbat"; Time = DateTimeOffset.FromUnixTimeSeconds 1700000000L }

/// Build a repo with a base commit (files), then main edits + feature edits.
/// Returns (workdir, mainTip, featTip).
let buildCase (idx: int) (baseFiles: (string * string) list) (mainFiles: (string * string) list) (featFiles: (string * string) list) =
    let wd = Path.Combine(tmp, "case" + string idx)
    Directory.CreateDirectory wd |> ignore
    git wd ["init"; "-q"; "-b"; "main"] |> ignore
    git wd ["config"; "user.email"; "t@t"] |> ignore
    git wd ["config"; "user.name"; "t"] |> ignore
    for (f, c) in baseFiles do File.WriteAllText(Path.Combine(wd, f), c)
    git wd ["add"; "-A"] |> ignore
    git wd ["commit"; "-q"; "-m"; "base"] |> ignore
    git wd ["checkout"; "-q"; "-b"; "feature"] |> ignore
    for (f, c) in featFiles do File.WriteAllText(Path.Combine(wd, f), c)
    git wd ["add"; "-A"] |> ignore
    git wd ["commit"; "-q"; "-m"; "feat"] |> ignore
    git wd ["checkout"; "-q"; "main"] |> ignore
    for (f, c) in mainFiles do File.WriteAllText(Path.Combine(wd, f), c)
    git wd ["add"; "-A"] |> ignore
    git wd ["commit"; "-q"; "-m"; "main"] |> ignore
    wd, revParse wd "main", revParse wd "feature"

// ---- Case 1: disjoint files (clean) ----
let wd1, main1, feat1 = buildCase 1 [ "a.txt", "aaa\n"; "b.txt", "bbb\n" ] [ "a.txt", "AAA\n" ] [ "b.txt", "BBB\n" ]
let g1code, g1out = git wd1 ["merge-tree"; "--write-tree"; "main"; "feature"]
let gitTree1 = g1out.Trim().Split('\n').[0]
let repo1 = match Repository.openRepo wd1 with Ok r -> r | Error e -> failwith e
match Merge3.merge repo1 main1 feat1 sig0 "merge" with
| Merge3.Merged mc ->
    let ourTree = match ReadObjects.readCommit repo1 mc with Ok c -> c.Tree | _ -> "?"
    check "disjoint-file merge is clean (git agrees)" (g1code = 0)
    check "merged tree OID matches git merge-tree" (ourTree = gitTree1)
    check "merge commit has two parents" (match ReadObjects.readCommit repo1 mc with Ok c -> c.Parents.Length = 2 | _ -> false)
| other -> check "disjoint-file merge clean" false; printfn "    got %A" other

// ---- Case 2: same file, non-overlapping line edits (diff3 clean) ----
let baseText = "l1\nl2\nl3\nl4\nl5\nl6\n"
let wd2, main2, feat2 =
    buildCase 2 [ "x.txt", baseText ]
        [ "x.txt", "L1\nl2\nl3\nl4\nl5\nl6\n" ]   // main edits first line
        [ "x.txt", "l1\nl2\nl3\nl4\nl5\nL6\n" ]   // feature edits last line
let g2code, g2out = git wd2 ["merge-tree"; "--write-tree"; "main"; "feature"]
let gitTree2 = g2out.Trim().Split('\n').[0]
let repo2 = match Repository.openRepo wd2 with Ok r -> r | Error e -> failwith e
match Merge3.merge repo2 main2 feat2 sig0 "merge" with
| Merge3.Merged mc ->
    let ourTree = match ReadObjects.readCommit repo2 mc with Ok c -> c.Tree | _ -> "?"
    check "diff3 non-overlapping merge clean (git agrees)" (g2code = 0)
    check "diff3 merged tree OID matches git" (ourTree = gitTree2)
| other -> check "diff3 non-overlapping clean" false; printfn "    got %A" other

// ---- Case 3: same file, same lines changed differently (conflict) ----
let wd3, main3, feat3 =
    buildCase 3 [ "x.txt", "hello\nworld\n" ]
        [ "x.txt", "HELLO\nworld\n" ]
        [ "x.txt", "GOODBYE\nworld\n" ]
let g3code, _ = git wd3 ["merge-tree"; "--write-tree"; "main"; "feature"]
let repo3 = match Repository.openRepo wd3 with Ok r -> r | Error e -> failwith e
check "git reports conflict on overlapping edits" (g3code <> 0)
match Merge3.merge repo3 main3 feat3 sig0 "merge" with
| Merge3.Conflicts paths -> check "Merge3 detects conflict" (List.contains "x.txt" paths)
| other -> check "Merge3 detects conflict" false; printfn "    got %A" other

try Directory.Delete(tmp, true) with _ -> ()
printfn ""
if failures = 0 then printfn "ALL F2 (MERGE) CHECKS PASSED" else (printfn "%d FAILED" failures; exit 1)
