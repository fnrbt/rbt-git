// M1 conformance test: fsgit object read/write vs canonical git.
// Run inside the dotnet SDK image (which includes git):
//   dotnet build src/FSharpGit/FSharpGit.fsproj -c Debug
//   dotnet fsi tests/M1Test.fsx
#I __SOURCE_DIRECTORY__
#r "../src/FSharpGit/bin/Debug/net10.0/FSharpGit.dll"

open System
open System.IO
open System.Diagnostics
open System.Text
open FSharpGit

let mutable failures = 0
let check name cond =
    if cond then printfn "  ok   - %s" name
    else (failures <- failures + 1; printfn "  FAIL - %s" name)

/// Run a process, return (exitCode, stdout). stdin optional bytes.
let run (workDir: string) (cmd: string) (args: string list) (stdin: byte[] option) : int * byte[] =
    let psi = ProcessStartInfo(cmd, RedirectStandardOutput = true, RedirectStandardInput = Option.isSome stdin,
                               UseShellExecute = false, WorkingDirectory = workDir)
    for a in args do psi.ArgumentList.Add a
    use p = Process.Start psi
    match stdin with
    | Some bytes -> p.StandardInput.BaseStream.Write(bytes, 0, bytes.Length); p.StandardInput.Close()
    | None -> ()
    use ms = new MemoryStream()
    p.StandardOutput.BaseStream.CopyTo ms
    p.WaitForExit()
    p.ExitCode, ms.ToArray()

let git workDir args = run workDir "git" args None
let gitStr workDir args = let _, o = git workDir args in Encoding.UTF8.GetString(o).TrimEnd('\n')

let tmp = Path.Combine(Path.GetTempPath(), "fsgit-m1-" + string (Guid.NewGuid().ToString("N").Substring(0,8)))
Directory.CreateDirectory tmp |> ignore
printfn "workdir: %s" tmp

// ---- 1. Real git creates a repo with a known blob + commit ----
let work = Path.Combine(tmp, "work")
Directory.CreateDirectory work |> ignore
git work ["init"; "-q"] |> ignore
git work ["config"; "user.email"; "t@t"] |> ignore
git work ["config"; "user.name"; "t"] |> ignore
let content = Encoding.UTF8.GetBytes "hello fsgit\n"
File.WriteAllBytes(Path.Combine(work, "a.txt"), content)
git work ["add"; "a.txt"] |> ignore
git work ["commit"; "-q"; "-m"; "first"] |> ignore
let gitBlobHash = gitStr work ["hash-object"; "a.txt"]
let gitHeadHash = gitStr work ["rev-parse"; "HEAD"]
printfn "git blob=%s head=%s" gitBlobHash gitHeadHash

// ---- 2. fsgit hashRaw matches git hash-object ----
check "hashRaw blob == git hash-object" (ObjectWriter.hashRaw "blob" content = gitBlobHash)

// ---- 3. fsgit reads real-git objects (zlib + path fix) ----
let repo = match Repository.openRepo work with Ok r -> r | Error e -> failwithf "openRepo: %s" e
match References.readHead repo with
| Ok (Symbolic r) -> check "readHead symbolic" (r.StartsWith "refs/heads/")
| other -> check "readHead symbolic" false; printfn "    got %A" other
check "readBlob roundtrips real-git blob" (match ReadObjects.readBlob repo gitBlobHash with Ok b -> b = content | _ -> false)
check "readCommit reads real-git HEAD" (match ReadObjects.readCommit repo gitHeadHash with Ok _ -> true | _ -> false)

// ---- 4. fsgit writes a loose object that REAL git can read back ----
let bare = Path.Combine(tmp, "bare.git")
let brepo = match Repository.initBare bare with Ok r -> r | Error e -> failwithf "initBare: %s" e
check "initBare HEAD exists" (File.Exists (Path.Combine(bare, "HEAD")))
let wrote = match ObjectWriter.writeBlob brepo content with Ok h -> h | Error e -> failwithf "writeBlob: %s" e
check "writeBlob hash == git hash" (wrote = gitBlobHash)
// real git, pointed at our bare repo, must be able to cat-file the object we wrote
let _, catBytes = run bare "git" ["--git-dir"; bare; "cat-file"; "-p"; wrote] None
check "real git cat-file reads fsgit-written blob" (catBytes = content)
let catType = (let _, o = run bare "git" ["--git-dir"; bare; "cat-file"; "-t"; wrote] None in Encoding.UTF8.GetString(o).Trim())
check "real git sees type=blob" (catType = "blob")

// ---- 5. readRawObject preserves exact bytes ----
check "readRawObject returns (blob, content)"
    (match ReadObjects.readRawObject brepo wrote with Ok ("blob", c) -> c = content | _ -> false)

// ---- 6. fsgit can re-read a real-git commit's tree, and write a tree git accepts ----
let treeHash = match ReadObjects.readCommit repo gitHeadHash with Ok c -> c.Tree | _ -> ""
let entries = match ReadObjects.readTree repo treeHash with Ok e -> e | _ -> [||]
let rewroteTree = match ObjectWriter.writeTree brepo entries with Ok h -> h | Error e -> failwithf "writeTree: %s" e
check "writeTree reproduces git tree hash" (rewroteTree = treeHash)

try Directory.Delete(tmp, true) with _ -> ()
printfn ""
if failures = 0 then printfn "ALL M1 CHECKS PASSED" else (printfn "%d CHECK(S) FAILED" failures; exit 1)
