module WorktreeRegression

open System
open System.IO
open System.Diagnostics
open Rbt.Git

let private check condition message = if not condition then failwith message
let private value = function Ok x -> x | Error error -> failwith error

let private git dir args =
    let start = ProcessStartInfo("git")
    start.WorkingDirectory <- dir
    start.RedirectStandardOutput <- true
    start.RedirectStandardError <- true
    for arg in args do start.ArgumentList.Add arg
    use gitProcess = Process.Start start
    let output = gitProcess.StandardOutput.ReadToEnd()
    let error = gitProcess.StandardError.ReadToEnd()
    gitProcess.WaitForExit()
    if gitProcess.ExitCode <> 0 then failwith $"git failed: {error}"
    output.TrimEnd('\r', '\n')

let run () =
    let dir = Path.Combine(Path.GetTempPath(), "rbt-worktree-" + Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory dir |> ignore
    try
        git dir [ "init"; "-q"; "-b"; "main" ] |> ignore
        git dir [ "config"; "user.name"; "Test" ] |> ignore
        git dir [ "config"; "user.email"; "test@example.invalid" ] |> ignore
        Directory.CreateDirectory(Path.Combine(dir, "nested")) |> ignore
        Directory.CreateDirectory(Path.Combine(dir, "rules")) |> ignore
        File.WriteAllText(Path.Combine(dir, ".gitignore"), "*.generated\n!important.generated\nbuild/\n")
        File.WriteAllText(Path.Combine(dir, "rules", ".gitignore"), "*.tmp\n!keep.tmp\n")
        File.WriteAllText(Path.Combine(dir, "nested", "old"), "first\nshared\n")
        File.WriteAllBytes(Path.Combine(dir, "bytes"), [| 0x80uy; 10uy; 0x82uy; 10uy |])
        git dir [ "add"; "." ] |> ignore
        git dir [ "commit"; "-qm"; "first" ] |> ignore
        let first = git dir [ "rev-parse"; "HEAD" ]
        git dir [ "checkout"; "-qb"; "other" ] |> ignore
        File.WriteAllText(Path.Combine(dir, "nested", "old"), "second\nshared\n")
        File.WriteAllText(Path.Combine(dir, "new"), "new\n")
        File.WriteAllBytes(Path.Combine(dir, "bytes"), [| 0x81uy; 10uy; 0x82uy; 10uy |])
        if not (OperatingSystem.IsWindows()) then
            File.WriteAllText(Path.Combine(dir, "executable"), "run\n")
            File.SetUnixFileMode(Path.Combine(dir, "executable"), enum<UnixFileMode> 0o755)
            File.CreateSymbolicLink(Path.Combine(dir, "link"), "nested/old") |> ignore
        git dir [ "add"; "." ] |> ignore
        git dir [ "commit"; "-qm"; "second" ] |> ignore
        let second = git dir [ "rev-parse"; "HEAD" ]
        let repo = Repository.openRepo dir |> value
        let initial = Diff.getChangedFiles repo first |> value
        check (initial |> Array.exists (fun c -> c.Path = "nested/old")) "initial tree did not enumerate leaves"
        check ((RepositoryOperations.getFileContent repo "nested/old" |> value |> System.Text.Encoding.UTF8.GetString) = "second\nshared\n") "file content used tree as commit"
        let blame = RepositoryOperations.blame repo "nested/old" |> value
        check (blame.Length = 2 && fst blame.[0] = second && fst blame.[1] = first) "blame attribution is incorrect"
        let byteBlame = RepositoryOperations.blame repo "bytes" |> value
        check (byteBlame.Length = 2 && fst byteBlame.[0] = second && fst byteBlame.[1] = first) "blame collapsed invalid UTF-8 bytes"
        check ((RepositoryOperations.listDirectory repo "." |> value).Length > 0) "dot did not resolve to root"
        check ((RepositoryOperations.getLog repo (Some 50) |> value).Length = 2) "log did not truncate"
        let changes = Diff.diffCommits repo first second |> value
        let changed = changes |> Array.find (fun c -> c.Path = "nested/old")
        let patch = Diff.getPatch repo changed |> value
        File.WriteAllText(Path.Combine(dir, "change.patch"), patch)
        git dir [ "apply"; "--reverse"; "--check"; "change.patch" ] |> ignore
        let binaryChange = changes |> Array.find (fun c -> c.Path = "bytes")
        check (Diff.getPatch repo binaryChange |> Result.isError) "patch silently replaced invalid UTF-8"
        let modeOnly =
            { Path = "nested/old"; OldHash = changed.NewHash; NewHash = changed.NewHash
              OldMode = Some 0o100644; NewMode = Some 0o100755; ChangeType = ChangeType.Modified }
        let modePatch = Diff.getPatch repo modeOnly |> value
        check (modePatch.Contains("old mode 100644") && modePatch.Contains("new mode 100755")) "mode patch missing modes"
        File.WriteAllText(Path.Combine(dir, "mode.patch"), modePatch)
        git dir [ "apply"; "--check"; "mode.patch" ] |> ignore
        File.Delete(Path.Combine(dir, "mode.patch"))
        let linkChange =
            { Path = "submodule"; OldHash = Some (String('1', 40)); NewHash = Some (String('2', 40))
              OldMode = Some 0o160000; NewMode = Some 0o160000; ChangeType = ChangeType.Modified }
        let linkPatch = Diff.getPatch repo linkChange |> value
        check (linkPatch.Contains("Subproject commit " + String('1', 40)) && linkPatch.Contains("Subproject commit " + String('2', 40))) "gitlink patch requires target objects"
        File.WriteAllText(Path.Combine(dir, "gitlink.patch"), linkPatch)
        git dir [ "update-index"; "--add"; "--cacheinfo"; "160000," + String('1', 40) + ",submodule" ] |> ignore
        git dir [ "apply"; "--cached"; "--check"; "gitlink.patch" ] |> ignore
        git dir [ "reset"; "-q"; "HEAD" ] |> ignore
        File.Delete(Path.Combine(dir, "gitlink.patch"))
        File.Delete(Path.Combine(dir, "change.patch"))
        check (RepositoryOperations.isClean repo |> value) "clean symbolic HEAD reported dirty"
        let excludes = Path.Combine(dir, ".git", "info", "global-excludes")
        File.WriteAllText(excludes, "global.artifact\n")
        git dir [ "config"; "core.excludesfile"; excludes ] |> ignore
        File.AppendAllText(Path.Combine(dir, ".git", "info", "exclude"), "\nlocal.artifact\n")
        Directory.CreateDirectory(Path.Combine(dir, "build")) |> ignore
        File.WriteAllText(Path.Combine(dir, "build", "output"), "ignored")
        File.WriteAllText(Path.Combine(dir, "artifact.generated"), "ignored")
        File.WriteAllText(Path.Combine(dir, "rules", "ignored.tmp"), "ignored")
        File.WriteAllText(Path.Combine(dir, "local.artifact"), "ignored")
        File.WriteAllText(Path.Combine(dir, "global.artifact"), "ignored")
        check (git dir [ "status"; "--porcelain"; "--untracked-files=all" ] = "") "Git did not ignore fixture files"
        check (RepositoryOperations.isClean repo |> value) "ignored files made status dirty"
        File.WriteAllText(Path.Combine(dir, "important.generated"), "visible")
        File.WriteAllText(Path.Combine(dir, "rules", "keep.tmp"), "visible")
        let gitStatus = git dir [ "status"; "--porcelain"; "--untracked-files=all" ]
        let status = RepositoryOperations.getStatus repo |> value
        let visible = status |> Array.map (fun entry -> entry.Path) |> Set.ofArray
        check (gitStatus.Contains("important.generated") && gitStatus.Contains("rules/keep.tmp")) "Git did not expose negated files"
        check (visible = Set.ofList [ "important.generated"; "rules/keep.tmp" ]) "negated ignore rules differ from Git"
        File.Delete(Path.Combine(dir, "important.generated"))
        File.Delete(Path.Combine(dir, "rules", "keep.tmp"))
        if not (OperatingSystem.IsWindows()) then
            git dir [ "config"; "core.filemode"; "false" ] |> ignore
            let path = Path.Combine(dir, "nested", "old")
            File.SetUnixFileMode(path, enum<UnixFileMode> 0o755)
            check (git dir [ "status"; "--porcelain" ] = "") "Git detected filemode with core.filemode=false"
            check (RepositoryOperations.isClean repo |> value) "status ignored core.filemode=false"
            let tree = (ReadObjects.readCommit repo second |> value).Tree
            check ((Diff.diffWorkingTree repo tree |> value).Length = 0) "diff ignored core.filemode=false"
            File.SetUnixFileMode(path, enum<UnixFileMode> 0o644)
            git dir [ "config"; "core.filemode"; "true" ] |> ignore
        let configBefore = File.ReadAllBytes(Path.Combine(dir, ".git", "config"))
        let blob = ObjectWriter.writeBlob repo (System.Text.Encoding.UTF8.GetBytes "malicious") |> value
        let baseCommit = ReadObjects.readCommit repo second |> value
        for name in [| ".git"; ".."; "/rooted"; "dir/../../outside" |] do
            let header = System.Text.Encoding.UTF8.GetBytes("100644 " + name + "\000")
            let rawTree = Array.append header (Convert.FromHexString blob)
            let tree = ObjectWriter.writeRaw repo "tree" rawTree |> value
            let badCommit = ObjectWriter.writeCommit repo { baseCommit with Tree = tree; Parents = [| second |] } |> value
            References.writeDirectAtomic repo "refs/heads/malicious" badCommit |> value
            check (RepositoryOperations.checkout repo "malicious" |> Result.isError) $"malicious checkout accepted {name}"
            check ((File.ReadAllBytes(Path.Combine(dir, ".git", "config"))) = configBefore) "malicious checkout changed git config"
            check ((RepositoryOperations.getHead repo |> value) = second) "malicious checkout changed HEAD"
        File.WriteAllText(Path.Combine(dir, "nested", "old"), "dirty\n")
        check (not (RepositoryOperations.isClean repo |> value)) "unstaged edit not detected"
        check (RepositoryOperations.checkout repo "main" |> Result.isError) "checkout overwrote a dirty file"
        check (File.ReadAllText(Path.Combine(dir, "nested", "old")) = "dirty\n") "checkout modified dirty file"
        git dir [ "restore"; "nested/old" ] |> ignore
        RepositoryOperations.checkout repo "main" |> value
        check (not (File.Exists(Path.Combine(dir, "new")))) "checkout did not remove old file"
        check (git dir [ "status"; "--porcelain" ] = "") "checkout index differs from Git"
        check (RepositoryOperations.currentBranch repo |> value = Some "main") "checkout did not update HEAD"
        if not (OperatingSystem.IsWindows()) then
            RepositoryOperations.checkout repo "other" |> value
            check (File.GetUnixFileMode(Path.Combine(dir, "executable")) &&& UnixFileMode.UserExecute <> enum<UnixFileMode>(0)) "executable mode lost"
            check ((FileInfo(Path.Combine(dir, "link"))).LinkTarget = "nested/old") "symlink mode lost"
            RepositoryOperations.checkout repo "main" |> value
        check (TreeOperations.safeWorkTreePath repo "../outside" |> Result.isError) "parent traversal accepted"
        check (TreeOperations.safeWorkTreePath repo ".git/config" |> Result.isError) "git directory traversal accepted"
        if not (OperatingSystem.IsWindows()) then
            let outside = dir + "-outside"
            Directory.CreateDirectory outside |> ignore
            try
                let victim = Path.Combine(outside, "victim")
                File.WriteAllText(victim, "untouched")
                let link = Path.Combine(dir, "escape")
                Directory.CreateSymbolicLink(link, outside) |> ignore
                check (TreeOperations.safeWorkTreePath repo "escape/victim" |> Result.isError) "symlink traversal accepted"
                let subtree = ObjectWriter.writeTree repo [| { Path = "victim"; Mode = 0o100644; Hash = blob } |] |> value
                let tree = ObjectWriter.writeTree repo [| { Path = "escape"; Mode = 0o040000; Hash = subtree } |] |> value
                let target = ObjectWriter.writeCommit repo { baseCommit with Tree = tree; Parents = [| first |] } |> value
                References.writeDirectAtomic repo "refs/heads/symlink-attack" target |> value
                check (RepositoryOperations.checkout repo "symlink-attack" |> Result.isError) "checkout followed symlink"
                check (File.ReadAllText victim = "untouched") "checkout modified symlink target"
                Directory.Delete link
            finally
                Directory.Delete(outside, true)
        git dir [ "checkout"; "-qb"; "file-transition" ] |> ignore
        File.Delete(Path.Combine(dir, "nested", "old"))
        Directory.Delete(Path.Combine(dir, "nested"))
        File.WriteAllText(Path.Combine(dir, "nested"), "replacement\n")
        git dir [ "add"; "-A" ] |> ignore
        git dir [ "commit"; "-qm"; "replace directory" ] |> ignore
        git dir [ "checkout"; "-q"; "main" ] |> ignore
        RepositoryOperations.checkout repo "file-transition" |> value
        check (File.ReadAllText(Path.Combine(dir, "nested")) = "replacement\n") "directory to file failed"
        RepositoryOperations.checkout repo "main" |> value
        check (File.Exists(Path.Combine(dir, "nested", "old"))) "file to directory failed"
        check (git dir [ "status"; "--porcelain" ] = "") "file/directory checkout has dirty index"
        File.WriteAllText(Path.Combine(dir, "nested", "old"), "staged\n")
        git dir [ "add"; "nested/old" ] |> ignore
        let staged = RepositoryOperations.getStagedChanges repo |> value
        check (staged |> Array.exists (fun e -> e.Path = "nested/old")) "staged change not detected"
        git dir [ "reset"; "-q"; "--hard"; "HEAD" ] |> ignore
        File.AppendAllText(Path.Combine(dir, ".git", "info", "exclude"), "new\n")
        File.WriteAllText(Path.Combine(dir, "new"), "untracked collision\n")
        check (RepositoryOperations.isClean repo |> value) "ignored collision appeared in status"
        check (RepositoryOperations.checkout repo "other" |> Result.isError) "checkout overwrote untracked file"
        check (File.ReadAllText(Path.Combine(dir, "new")) = "untracked collision\n") "untracked file was modified"
        File.WriteAllText(Path.Combine(dir, "untracked"), "x")
        check (not (RepositoryOperations.isClean repo |> value)) "untracked file not detected"
        File.Delete(Path.Combine(dir, "new"))
        RepositoryOperations.checkout repo "other" |> value
        File.Delete(Repository.getLooseObjectPath repo first)
        check ((RepositoryOperations.getLog repo (Some 1) |> value).Length = 1) "bounded log read beyond its limit"
        check (RepositoryOperations.getLog repo None |> Result.isError) "log did not report missing history"
    finally
        if Directory.Exists dir then Directory.Delete(dir, true)
