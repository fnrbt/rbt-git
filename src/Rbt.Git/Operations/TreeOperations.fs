namespace Rbt.Git

open System
open System.IO

module TreeOperations =
    /// Read effective core settings from system, user and repository config.
    let coreSetting (repo: Repo) (key: string) : Result<string option, string> =
        try
            let env (name: string) = Environment.GetEnvironmentVariable name
            let home = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
            let xdg =
                match env "XDG_CONFIG_HOME" with
                | null | "" -> Path.Combine(home, ".config")
                | path -> path
            let paths = ResizeArray<string>()
            if String.IsNullOrEmpty(env "GIT_CONFIG_NOSYSTEM") then
                match env "GIT_CONFIG_SYSTEM" with
                | null | "" -> paths.Add "/etc/gitconfig"
                | path -> paths.Add path
            match env "GIT_CONFIG_GLOBAL" with
            | null | "" ->
                paths.Add(Path.Combine(xdg, "git", "config"))
                paths.Add(Path.Combine(home, ".gitconfig"))
            | path -> paths.Add path
            paths.Add(Repository.getConfigFile repo)
            let mutable result = None
            for path in paths do
                if File.Exists path then
                    let config = GitConfig.parseConfig (File.ReadAllText path)
                    match GitConfig.getValue config "core" None key with
                    | Some value -> result <- Some value
                    | None -> ()
            Ok result
        with ex -> Error $"Failed to read core.{key}: {ex.Message}"

    let honorsFileMode (repo: Repo) : Result<bool, string> =
        coreSetting repo "filemode"
        |> Result.bind (function
            | None -> Ok true
            | Some setting ->
                match setting.Trim().ToLowerInvariant() with
                | "true" | "yes" | "on" | "1" -> Ok true
                | "false" | "no" | "off" | "0" -> Ok false
                | other -> Error $"Invalid core.filemode value: {other}")

    /// Validate a Git path before accessing the worktree. Never follow a
    /// symlink in an ancestor, even if the target would remain inside it.
    let safeWorkTreePath (repo: Repo) (path: string) : Result<string, string> =
        try
            let parts = path.Split('/')
            if repo.WorkTree.IsNone || String.IsNullOrEmpty path || Path.IsPathRooted path
               || path.Contains('\\') || (parts |> Array.exists (fun p ->
                   p = "" || p = "." || p = ".." || p.Equals(".git", StringComparison.OrdinalIgnoreCase)
                   || p.IndexOfAny(Path.GetInvalidFileNameChars()) >= 0 || p.Contains(':'))) then
                Error $"Unsafe worktree path: {path}"
            else
                let mutable current = repo.WorkTree.Value
                let mutable unsafeParent = false
                for i = 0 to parts.Length - 2 do
                    current <- Path.Combine(current, parts.[i])
                    let info = FileInfo current
                    if not (isNull info.LinkTarget) then unsafeParent <- true
                if unsafeParent then Error $"Symlink in worktree path: {path}"
                else Ok (Path.Combine(repo.WorkTree.Value, path.Replace('/', Path.DirectorySeparatorChar)))
        with ex -> Error $"Invalid worktree path {path}: {ex.Message}"

    
    let rec private resolvePathInternal (repo: Repo) (tree: GitHash) (pathParts: string[]) (index: int) : Result<GitObject * string, string> =
        if index >= pathParts.Length then
            match ReadObjects.readObject repo tree with
            | Ok obj -> Ok (obj, tree)
            | Error msg -> Error msg
        else
            let part = pathParts.[index]
            match ReadObjects.readTree repo tree with
            | Ok entries ->
                let entry = entries |> Array.tryFind (fun e -> e.Path = part)
                match entry with
                | Some e ->
                    if index = pathParts.Length - 1 then
                        ReadObjects.readObject repo e.Hash |> Result.map (fun obj -> (obj, e.Hash))
                    else
                        resolvePathInternal repo e.Hash pathParts (index + 1)
                | None -> Error $"Path not found: {part}"
            | Error msg -> Error msg
    
    let resolvePath (repo: Repo) (tree: GitHash) (path: string) : Result<GitObject * string, string> =
        let normalizedPath = path.Replace('\\', '/').TrimStart('/')
        if String.IsNullOrWhiteSpace normalizedPath || normalizedPath = "." then
            ReadObjects.readObject repo tree |> Result.map (fun obj -> (obj, tree))
        else
            let parts = normalizedPath.Split('/')
            resolvePathInternal repo tree parts 0
    
    let listTree (repo: Repo) (tree: GitHash) : Result<TreeEntry[], string> =
        ReadObjects.readTree repo tree
    
    let rec private collectEntries (repo: Repo) (tree: GitHash) (prefix: string) (acc: ResizeArray<TreeEntry>) : Result<unit, string> =
        match ReadObjects.readTree repo tree with
        | Error msg -> Error msg
        | Ok entries ->
            let rec loop i =
                if i = entries.Length then Ok ()
                else
                    let entry = entries.[i]
                    let fullPath = if String.IsNullOrEmpty prefix then entry.Path else $"{prefix}/{entry.Path}"
                    let result =
                        if entry.Mode &&& 0o170000 = 0o040000 then
                            collectEntries repo entry.Hash fullPath acc
                        else
                            acc.Add { entry with Path = fullPath }
                            Ok ()
                    result |> Result.bind (fun () -> loop (i + 1))
            loop 0

    let listTreeEntriesRecursive (repo: Repo) (tree: GitHash) : Result<TreeEntry[], string> =
        let entries = ResizeArray()
        collectEntries repo tree "" entries |> Result.map (fun () -> entries.ToArray())

    let listTreeRecursive (repo: Repo) (tree: GitHash) : Result<(string * GitHash)[], string> =
        listTreeEntriesRecursive repo tree |> Result.map (Array.map (fun e -> e.Path, e.Hash))
    
    let getTreeAtCommit (repo: Repo) (commit: GitHash) : Result<TreeEntry[], string> =
        match ReadObjects.readCommit repo commit with
        | Ok commitData -> ReadObjects.readTree repo commitData.Tree
        | Error msg -> Error msg
    
    let getFileAtCommit (repo: Repo) (commit: GitHash) (path: string) : Result<byte[], string> =
        match ReadObjects.readCommit repo commit with
        | Ok commitData ->
            match resolvePath repo commitData.Tree path with
            | Ok (Blob data, _) -> Ok data
            | Ok _ -> Error $"Path is not a file: {path}"
            | Error msg -> Error msg
        | Error msg -> Error msg
    
    let getTreeAtPath (repo: Repo) (tree: GitHash) (path: string) : Result<TreeEntry[], string> =
        match resolvePath repo tree path with
        | Ok (Tree entries, _) -> Ok entries
        | Ok _ -> Error $"Path is not a tree: {path}"
        | Error msg -> Error msg
    
    let pathExists (repo: Repo) (tree: GitHash) (path: string) : bool =
        match resolvePath repo tree path with
        | Ok _ -> true
        | _ -> false
    
    let isDirectory (repo: Repo) (tree: GitHash) (path: string) : bool =
        match resolvePath repo tree path with
        | Ok (Tree _, _) -> true
        | Ok _ -> false
        | _ -> false
    
    let isFile (repo: Repo) (tree: GitHash) (path: string) : bool =
        match resolvePath repo tree path with
        | Ok (Blob _, _) -> true
        | Ok _ -> false
        | _ -> false
    
    let getTreeHashAtCommit (repo: Repo) (commit: GitHash) : Result<GitHash, string> =
        match ReadObjects.readCommit repo commit with
        | Ok commitData -> Ok commitData.Tree
        | Error msg -> Error msg
    
    let getSubtreeHash (repo: Repo) (tree: GitHash) (path: string) : Result<GitHash, string> =
        match resolvePath repo tree path with
        | Ok (Tree _, hash) -> Ok hash
        | Ok _ -> Error $"Path is not a tree: {path}"
        | Error msg -> Error msg
    
    let getBlobHash (repo: Repo) (tree: GitHash) (path: string) : Result<GitHash, string> =
        match resolvePath repo tree path with
        | Ok (Blob _, hash) -> Ok hash
        | Ok _ -> Error $"Path is not a blob: {path}"
        | Error msg -> Error msg
    
    let treeToMap (repo: Repo) (tree: GitHash) : Result<System.Collections.Generic.Dictionary<string, GitHash>, string> =
        listTreeRecursive repo tree
        |> Result.map (fun entries ->
            let map = System.Collections.Generic.Dictionary<string, GitHash>(StringComparer.Ordinal)
            for path, hash in entries do map.Add(path, hash)
            map)
