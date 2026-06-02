namespace FSharpGit

open System
open System.IO

module TreeOperations =
    
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
        if String.IsNullOrWhiteSpace normalizedPath then
            ReadObjects.readObject repo tree |> Result.map (fun obj -> (obj, tree))
        else
            let parts = normalizedPath.Split('/')
            resolvePathInternal repo tree parts 0
    
    let listTree (repo: Repo) (tree: GitHash) : Result<TreeEntry[], string> =
        ReadObjects.readTree repo tree
    
    let rec private listTreeRecursiveInternal (repo: Repo) (tree: GitHash) (prefix: string) (acc: ResizeArray<string * GitHash>) : Result<ResizeArray<string * GitHash>, string> =
        match ReadObjects.readTree repo tree with
        | Ok entries ->
            for entry in entries do
                let fullPath = if String.IsNullOrEmpty prefix then entry.Path else $"{prefix}/{entry.Path}"
                acc.Add ((fullPath, entry.Hash))
                
                if entry.Mode &&& 0o40000 <> 0 then
                    match listTreeRecursiveInternal repo entry.Hash fullPath acc with
                    | Ok _ -> ()
                    | Error msg -> Error msg |> ignore
            Ok acc
        | Error msg -> Error msg
    
    let listTreeRecursive (repo: Repo) (tree: GitHash) : Result<(string * GitHash)[], string> =
        match listTreeRecursiveInternal repo tree "" (ResizeArray()) with
        | Ok acc -> Ok (acc.ToArray())
        | Error msg -> Error msg
    
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
        | Ok (Blob _, _) -> Error $"Path is a file, not a tree: {path}"
        | Error msg -> Error msg
    
    let getBlobHash (repo: Repo) (tree: GitHash) (path: string) : Result<GitHash, string> =
        match resolvePath repo tree path with
        | Ok (Blob _, hash) -> Ok hash
        | Ok (Tree _, _) -> Error $"Path is a tree, not a blob: {path}"
        | Error msg -> Error msg
    
    let rec private treeToMapInternal (repo: Repo) (tree: GitHash) (prefix: string) (map: System.Collections.Generic.Dictionary<string, GitHash>) : Result<unit, string> =
        match ReadObjects.readTree repo tree with
        | Ok entries ->
            for entry in entries do
                let fullPath = if String.IsNullOrEmpty prefix then entry.Path else $"{prefix}/{entry.Path}"
                map.[fullPath] <- entry.Hash
                
                if entry.Mode &&& 0o40000 <> 0 then
                    match treeToMapInternal repo entry.Hash fullPath map with
                    | Ok _ -> ()
                    | Error msg -> Error msg |> ignore
            Ok ()
        | Error msg -> Error msg
    
    let treeToMap (repo: Repo) (tree: GitHash) : Result<System.Collections.Generic.Dictionary<string, GitHash>, string> =
        let map = System.Collections.Generic.Dictionary()
        match treeToMapInternal repo tree "" map with
        | Ok () -> Ok map
        | Error msg -> Error msg
