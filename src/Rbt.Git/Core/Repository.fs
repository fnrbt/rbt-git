namespace Rbt.Git

open System
open System.IO

module Repository =
    
    let locateRepo (path: string) : Repo =
        let fullPath = Path.GetFullPath path
        let start = if Directory.Exists fullPath then fullPath else Path.GetDirectoryName fullPath
        let rec find current =
            let dotGit = Path.Combine(current, ".git")
            if Directory.Exists dotGit then
                { Path = current; GitDir = Path.GetFullPath dotGit; WorkTree = Some current }
            elif File.Exists dotGit then
                let content = File.ReadAllText(dotGit).Trim()
                if not (content.StartsWith("gitdir:", StringComparison.OrdinalIgnoreCase)) then
                    raise (InvalidDataException $"Invalid gitdir file: {dotGit}")
                let target = content.Substring(7).Trim()
                if String.IsNullOrWhiteSpace target then
                    raise (InvalidDataException $"Invalid gitdir file: {dotGit}")
                let gitDir = Path.GetFullPath(target, current)
                if not (Directory.Exists gitDir) then
                    raise (DirectoryNotFoundException $"Git directory not found: {gitDir}")
                { Path = current; GitDir = gitDir; WorkTree = Some current }
            elif File.Exists(Path.Combine(current, "HEAD")) && Directory.Exists(Path.Combine(current, "objects")) then
                { Path = current; GitDir = current; WorkTree = None }
            else
                let parent = Directory.GetParent current
                if isNull parent then raise (DirectoryNotFoundException $"Not a git repository: {path}")
                find parent.FullName
        find start
    
    let openRepo (path: string) : Result<Repo, string> =
        try
            Ok (locateRepo path)
        with
        | :? DirectoryNotFoundException as ex -> Error ex.Message
        | ex -> Error $"Failed to open repository: {ex.Message}"
    
    let gitPath (repo: Repo) (segments: string[]) : string =
        Path.Combine(Array.append [| repo.GitDir |] segments)

    /// Linked worktrees keep shared objects, refs and configuration in commondir.
    let getCommonDir (repo: Repo) =
        let file = Path.Combine(repo.GitDir, "commondir")
        if File.Exists file then
            let value = File.ReadAllText(file).Trim()
            if String.IsNullOrWhiteSpace value then raise (InvalidDataException $"Invalid commondir: {file}")
            Path.GetFullPath(value, repo.GitDir)
        else repo.GitDir

    let private commonPath (repo: Repo) (segments: string[]) =
        Path.Combine(Array.append [| getCommonDir repo |] segments)

    let hasWorkTree (repo: Repo) : bool =
        Option.isSome repo.WorkTree

    let workTreePath (repo: Repo) (segments: string[]) : string =
        match repo.WorkTree with
        | Some wt -> Path.Combine(Array.append [| wt |] segments)
        | None ->
            raise (NotSupportedException "Bare repository does not have a working tree")
    
    let isBare (repo: Repo) : bool =
        not (hasWorkTree repo)
    
    let getHeadFile (repo: Repo) : string =
        gitPath repo [| "HEAD" |]
    
    let getConfigFile (repo: Repo) : string =
        commonPath repo [| "config" |]
    
    let getIndexPath (repo: Repo) : string =
        gitPath repo [| "index" |]
    
    let getObjectsDir (repo: Repo) : string =
        commonPath repo [| "objects" |]
    
    let getRefsDir (repo: Repo) : string =
        commonPath repo [| "refs" |]
    
    let getHeadsDir (repo: Repo) : string =
        Path.Combine(getRefsDir repo, "heads")
    
    let getTagsDir (repo: Repo) : string =
        Path.Combine(getRefsDir repo, "tags")
    
    let getRemotesDir (repo: Repo) : string =
        Path.Combine(getRefsDir repo, "remotes")
    
    let getPackedRefsFile (repo: Repo) : string =
        commonPath repo [| "packed-refs" |]
    
    let getPackDir (repo: Repo) : string =
        Path.Combine(getObjectsDir repo, "pack")
    
    let getLooseObjectPath (repo: Repo) (hash: GitHash) : string =
        let dir = hash.[..1]
        let file = hash.[2..]
        Path.Combine(getObjectsDir repo, dir, file)
    
    let getPackIndexPath (repo: Repo) (packName: string) : string =
        Path.Combine(getPackDir repo, $"{packName}.idx")
    
    let getPackFilePath (repo: Repo) (packName: string) : string =
        Path.Combine(getPackDir repo, $"{packName}.pack")
    
    let packFilesExist (repo: Repo) : (string * string) [] =
        if not (Directory.Exists (getPackDir repo)) then
            [||]
        else
            Directory.GetFiles(getPackDir repo, "*.idx")
            |> Array.map (fun idxPath ->
                let name = Path.GetFileNameWithoutExtension idxPath
                let packPath = Path.ChangeExtension(idxPath, ".pack")
                (name, packPath))
    
    let branchFilePath (repo: Repo) (branch: string) : string =
        Path.Combine(getHeadsDir repo, branch)
    
    let tagFilePath (repo: Repo) (tag: string) : string =
        Path.Combine(getTagsDir repo, tag)
    
    let remoteBranchFilePath (repo: Repo) (remote: string) (branch: string) : string =
        Path.Combine(getRemotesDir repo, remote, branch)

    /// Open an existing bare repository (the directory itself is the git dir).
    let openBare (path: string) : Result<Repo, string> =
        let full = Path.GetFullPath path
        if Directory.Exists full && File.Exists (Path.Combine(full, "HEAD")) then
            Ok { Path = full; GitDir = full; WorkTree = None }
        else
            Error $"Not a bare repository: {path}"

    /// Initialize a new empty bare repository at the given path.
    let initBareWithDefaultBranch (path: string) (defaultBranch: string) : Result<Repo, string> =
        try
            let refName = "refs/heads/" + defaultBranch
            let validBranch =
                not (String.IsNullOrWhiteSpace defaultBranch)
                && not (defaultBranch.EndsWith(".", StringComparison.Ordinal))
                && not (defaultBranch.Contains("..", StringComparison.Ordinal))
                && not (defaultBranch.Contains("@{", StringComparison.Ordinal))
                && (defaultBranch.Split('/') |> Array.forall (fun p -> p <> "" && not (p.StartsWith(".")) && not (p.EndsWith(".lock", StringComparison.OrdinalIgnoreCase))))
                && not (defaultBranch |> Seq.exists (fun c -> Char.IsControl c || Char.IsWhiteSpace c || "\\~^:?*[".Contains(c)))
            if not validBranch then
                Error $"Invalid default branch: {defaultBranch}"
            else
                let full = Path.GetFullPath path
                Directory.CreateDirectory full |> ignore
                Directory.CreateDirectory (Path.Combine(full, "objects", "pack")) |> ignore
                Directory.CreateDirectory (Path.Combine(full, "refs", "heads")) |> ignore
                Directory.CreateDirectory (Path.Combine(full, "refs", "tags")) |> ignore
                let createIfMissing (file: string) (content: string) =
                    try
                        use stream = new FileStream(file, FileMode.CreateNew, FileAccess.Write, FileShare.None)
                        use writer = new StreamWriter(stream)
                        writer.Write(content)
                    with :? IOException when File.Exists file -> ()
                createIfMissing (Path.Combine(full, "HEAD")) $"ref: {refName}\n"
                createIfMissing (Path.Combine(full, "config")) "[core]\n\trepositoryformatversion = 0\n\tfilemode = true\n\tbare = true\n"
                Ok { Path = full; GitDir = full; WorkTree = None }
        with ex -> Error $"Failed to init bare repository at {Path.GetFullPath path}: {ex.Message}"

    let initBare (path: string) : Result<Repo, string> =
        initBareWithDefaultBranch path "main"
