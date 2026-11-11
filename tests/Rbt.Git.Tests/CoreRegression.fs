module CoreRegression

open System
open System.IO
open System.Diagnostics
open Rbt.Git

let private value = function Ok x -> x | Error e -> failwith e
let private check condition message = if not condition then failwith message
let private git dir args =
    let info = ProcessStartInfo("git")
    info.WorkingDirectory <- dir
    info.RedirectStandardOutput <- true
    info.RedirectStandardError <- true
    for arg in args do info.ArgumentList.Add arg
    use p = Process.Start info
    let output = p.StandardOutput.ReadToEnd()
    let error = p.StandardError.ReadToEnd()
    p.WaitForExit()
    if p.ExitCode <> 0 then failwith $"git failed: {error}"
    output.Trim()

let run () =
    let root = Path.Combine(Path.GetTempPath(), "rbt-core-" + Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory root |> ignore
    try
        git root [ "init"; "-q"; "main" ] |> ignore
        let work = Path.Combine(root, "main")
        git work [ "-c"; "user.name=Test"; "-c"; "user.email=test@example.invalid"; "commit"; "--allow-empty"; "-qm"; "initial" ] |> ignore
        let hash = git work [ "rev-parse"; "HEAD" ]
        let nested = Path.Combine(work, "nested", "deep")
        Directory.CreateDirectory nested |> ignore
        let repo = Repository.openRepo nested |> value
        check (repo.WorkTree = Some work) "nested repository root is incorrect"
        let linked = Path.Combine(root, "linked")
        git work [ "worktree"; "add"; "-qb"; "linked-test"; linked ] |> ignore
        let linkedRepo = Repository.openRepo linked |> value
        check (linkedRepo.WorkTree = Some linked) "linked worktree root is incorrect"
        check (Repository.getObjectsDir linkedRepo = Repository.getObjectsDir repo) "linked objects not shared"
        check (References.resolveReference linkedRepo "heads/linked-test" |> value = hash) "linked shared ref not resolved"
        let bare = Path.Combine(root, "bare.git")
        let br = Repository.initBare bare |> value
        let oldHead = File.ReadAllText(Repository.getHeadFile br)
        let oldConfig = File.ReadAllText(Repository.getConfigFile br)
        Repository.initBareWithDefaultBranch bare "other" |> value |> ignore
        check (File.ReadAllText(Repository.getHeadFile br) = oldHead && File.ReadAllText(Repository.getConfigFile br) = oldConfig) "reinit changed existing repository"
        check (Repository.openRepo bare |> value |> Repository.isBare) "bare open not recognized"
        let other = String.replicate 40 "a"
        check (References.isValidName "refs/foo") "two-component full ref rejected"
        References.compareAndSwap br "refs/foo" None (Some hash) |> value
        check (References.resolveReference br "refs/foo" |> value = hash) "two-component full ref not resolved"
        References.createBranch br "feature/nested" hash |> value
        check (References.createBranch br "feature/nested" other |> Result.isError) "create overwrote branch"
        check (References.createBranch br "../escape" hash |> Result.isError) "invalid branch accepted"
        check (References.createBranch br "bad" "not-a-hash" |> Result.isError) "invalid hash accepted"
        let refFile = Repository.branchFilePath br "feature/nested"
        check (References.compareAndSwap br "refs/heads/feature/nested" (Some other) (Some other) |> Result.isError) "CAS accepted stale old value"
        let outside = Path.Combine(root, "outside")
        Directory.CreateDirectory outside |> ignore
        Directory.CreateSymbolicLink(Path.Combine(Repository.getHeadsDir br, "link"), outside) |> ignore
        check (References.createBranch br "link/escaped" hash |> Result.isError) "ref write followed symlink"
        check (not (File.Exists(Path.Combine(outside, "escaped")))) "ref escaped repository"
        Directory.Delete(Path.Combine(Repository.getHeadsDir br, "link"))
        File.WriteAllText(refFile + ".lock", "owned")
        check (References.updateBranch br "feature/nested" other |> Result.isError) "ref lock ignored"
        File.Delete(refFile + ".lock")
        File.WriteAllText(Repository.getPackedRefsFile br, $"# pack-refs with: peeled\n{other} refs/heads/feature/nested\n^{hash}\n{hash} refs/tags/keep\n^{other}\n")
        check (References.resolveReference br "refs/heads/feature/nested" |> value = hash) "loose ref did not override packed"
        let alias = Path.Combine(Repository.getHeadsDir br, "alias")
        File.WriteAllText(alias, "ref: refs/heads/feature/nested\n")
        File.WriteAllText(refFile + ".lock", "not a ref")
        let snapshot = References.snapshot br RefPolicy.Replication |> value
        check (snapshot.Refs.["refs/heads/feature/nested"] = hash) "snapshot ignored loose override"
        check (snapshot.Refs.["refs/heads/alias"] = hash) "snapshot failed to resolve symbolic ref"
        check (snapshot.Refs.["refs/tags/keep"] = hash) "snapshot missed packed ref"
        check (not (snapshot.Refs.ContainsKey "refs/heads/feature/nested.lock")) "snapshot included lock file"
        File.Delete(refFile + ".lock")
        File.Delete alias
        check (References.findRef br hash |> value |> Option.isSome) "findRef missed ref"
        let packedLock = Repository.getPackedRefsFile br + ".lock"
        File.WriteAllText(packedLock, "owned")
        check (References.deleteBranch br "feature/nested" |> Result.isError) "packed refs lock ignored"
        check (File.Exists refFile) "failed delete removed loose ref"
        File.Delete packedLock
        References.deleteBranch br "feature/nested" |> value
        check (References.resolveReference br "heads/feature/nested" |> Result.isError) "packed ref resurrected"
        let packed = File.ReadAllText(Repository.getPackedRefsFile br)
        check (not (packed.Contains("refs/heads/feature/nested")) && packed.Contains("refs/tags/keep") && packed.Contains("^" + other)) "packed deletion damaged peeled entries"
        File.WriteAllText(Path.Combine(Repository.getHeadsDir br, "cycle-a"), "ref: refs/heads/cycle-b\n")
        File.WriteAllText(Path.Combine(Repository.getHeadsDir br, "cycle-b"), "ref: refs/heads/cycle-a\n")
        check (References.resolveReference br "heads/cycle-a" |> Result.isError) "symbolic cycle accepted"
        let configFile = Repository.getConfigFile br
        File.WriteAllText(configFile, "[Remote \"origin\"]\n url = \"a # b\" # comment\n fetch = one\n fetch = two\n[core]\n bare\n")
        let config = GitConfig.readConfig br |> value
        check (GitConfig.getValues config "remote" (Some "origin") "FETCH" = [| "one"; "two" |]) "repeated config values lost"
        check (GitConfig.getValue config "remote" (Some "origin") "url" = Some "a # b") "quoted config parsed incorrectly"
        check (GitConfig.getValue config "core" None "bare" = Some "true") "implicit boolean parsed incorrectly"
        GitConfig.updateValue br "core" None "filemode" "false" |> value
        let config = GitConfig.readConfig br |> value
        check (GitConfig.getValues config "REMOTE" (Some "origin") "fetch" = [| "one"; "two" |]) "config update lost repeated values"
        let original = File.ReadAllText configFile
        check (GitConfig.updateConfig br (fun _ -> Error "rejected") = Error "rejected") "config transform error not preserved"
        check (File.ReadAllText configFile = original) "rejected config transform wrote file"
        check (not (File.Exists(configFile + ".lock"))) "rejected config transform retained lock"
        File.WriteAllText(configFile + ".lock", "owned")
        check (GitConfig.updateValue br "core" None "bare" "false" |> Result.isError) "config lock ignored"
    finally
        if Directory.Exists root then Directory.Delete(root, true)
