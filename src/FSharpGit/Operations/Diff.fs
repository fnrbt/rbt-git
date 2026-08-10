namespace FSharpGit

open System.Collections.Generic

module Diff =
    
    let private mapTree (repo: Repo) (tree: GitHash) : Result<Dictionary<string, GitHash>, string> =
        TreeOperations.treeToMap repo tree
    
    let rec private diffTreesInternal (entries1: TreeEntry[]) (entries2: TreeEntry[]) (path: string) (repo: Repo) (changes: ResizeArray<FileChange>) : Result<ResizeArray<FileChange>, string> =
        let map1 = entries1 |> Array.map (fun e -> e.Path, e) |> dict
        let map2 = entries2 |> Array.map (fun e -> e.Path, e) |> dict
        let names =
            Array.append (entries1 |> Array.map (fun e -> e.Path)) (entries2 |> Array.map (fun e -> e.Path))
            |> Array.distinct
        let isTree (e: TreeEntry) = e.Mode &&& 0o40000 <> 0
        let full (name: string) = if System.String.IsNullOrEmpty path then name else $"{path}/{name}"
        // Recurse into a subtree present on one or both sides. Only leaf (blob)
        // differences are emitted; directories themselves are never reported,
        // matching `git diff`.
        let recurseSub (h1: GitHash option) (h2: GitHash option) (name: string) =
            let load (h: GitHash option) =
                match h with
                | Some hh -> (match TreeOperations.listTree repo hh with Ok es -> es | Error _ -> [||])
                | None -> [||]
            diffTreesInternal (load h1) (load h2) (full name) repo changes |> ignore
        for name in names do
            let e1 = if map1.ContainsKey name then Some map1.[name] else None
            let e2 = if map2.ContainsKey name then Some map2.[name] else None
            match e1, e2 with
            | Some a, Some b when a.Hash = b.Hash && a.Mode = b.Mode -> ()
            | Some a, Some b ->
                match isTree a, isTree b with
                | true, true -> recurseSub (Some a.Hash) (Some b.Hash) name
                | false, false ->
                    changes.Add { Path = full name; OldHash = Some a.Hash; NewHash = Some b.Hash; ChangeType = Modified }
                | true, false ->
                    recurseSub (Some a.Hash) None name
                    changes.Add { Path = full name; OldHash = None; NewHash = Some b.Hash; ChangeType = Added }
                | false, true ->
                    changes.Add { Path = full name; OldHash = Some a.Hash; NewHash = None; ChangeType = Deleted }
                    recurseSub None (Some b.Hash) name
            | Some a, None ->
                if isTree a then recurseSub (Some a.Hash) None name
                else changes.Add { Path = full name; OldHash = Some a.Hash; NewHash = None; ChangeType = Deleted }
            | None, Some b ->
                if isTree b then recurseSub None (Some b.Hash) name
                else changes.Add { Path = full name; OldHash = None; NewHash = Some b.Hash; ChangeType = Added }
            | None, None -> ()
        Ok changes
    
    let diffTrees (repo: Repo) (oldTree: GitHash) (newTree: GitHash) : Result<FileChange[], string> =
        match TreeOperations.listTree repo oldTree, TreeOperations.listTree repo newTree with
        | Ok entries1, Ok entries2 ->
            match diffTreesInternal entries1 entries2 "" repo (ResizeArray()) with
            | Ok changes -> Ok (changes.ToArray())
            | Error msg -> Error msg
        | Error msg, _ -> Error msg
        | _, Error msg -> Error msg
    
    let diffCommits (repo: Repo) (oldCommit: GitHash) (newCommit: GitHash) : Result<FileChange[], string> =
        match ReadObjects.readCommit repo oldCommit, ReadObjects.readCommit repo newCommit with
        | Ok oldData, Ok newData ->
            diffTrees repo oldData.Tree newData.Tree
        | Error msg, _ -> Error msg
        | _, Error msg -> Error msg
    
    let getChangedFiles (repo: Repo) (commit: GitHash) : Result<FileChange[], string> =
        match ReadObjects.readCommit repo commit with
        | Ok commitData when Array.isEmpty commitData.Parents ->
            match TreeOperations.listTree repo commitData.Tree with
            | Ok entries ->
                let changes = 
                    entries
                    |> Array.map (fun entry -> {
                        Path = entry.Path
                        OldHash = None
                        NewHash = Some entry.Hash
                        ChangeType = Added
                    })
                Ok changes
            | Error msg -> Error msg
        | Ok commitData ->
            let parent = commitData.Parents.[0]
            match ReadObjects.readCommit repo parent with
            | Ok parentData ->
                diffTrees repo parentData.Tree commitData.Tree
            | Error msg -> Error msg
        | Error msg -> Error msg
    
    let diffWorkingTree (repo: Repo) (tree: GitHash) : Result<FileChange[], string> =
        if not (Repository.hasWorkTree repo) then
            Error "Cannot diff working tree of bare repository"
        else
            match IndexParser.readIndex repo with
            | Ok index ->
                let workTree = repo.WorkTree.Value
                let changes = ResizeArray()
                
                for entry in index.Entries do
                    let fullPath = System.IO.Path.Combine(workTree, entry.Path.Replace('/', System.IO.Path.DirectorySeparatorChar))
                    
                    if not (System.IO.File.Exists fullPath) then
                        changes.Add {
                            Path = entry.Path
                            OldHash = Some entry.Hash
                            NewHash = None
                            ChangeType = Deleted
                        }
                    else
                        let content = System.IO.File.ReadAllBytes fullPath
                        let hash = Hashing.hashBlob content
                        
                        if hash <> entry.Hash then
                            changes.Add {
                                Path = entry.Path
                                OldHash = Some entry.Hash
                                NewHash = Some hash
                                ChangeType = Modified
                            }
                
                match TreeOperations.treeToMap repo tree with
                | Ok treeMap ->
                    for entry in index.Entries do
                        if treeMap.ContainsKey entry.Path then
                            treeMap.Remove entry.Path |> ignore
                    
                    for path in treeMap.Keys do
                        changes.Add {
                            Path = path
                            OldHash = Some treeMap.[path]
                            NewHash = None
                            ChangeType = Deleted
                        }
                    Ok (changes.ToArray())
                | Error msg -> Error msg
            | Error msg -> Error msg
    
    let getPatch (repo: Repo) (change: FileChange) : Result<string, string> =
        let getBlobContent (hashOpt: GitHash option) : Result<byte[] option, string> =
            match hashOpt with
            | Some hash ->
                match ReadObjects.readBlob repo hash with
                | Ok data -> Ok (Some data)
                | Error msg -> Error msg
            | None -> Ok None
        
        match getBlobContent change.OldHash, getBlobContent change.NewHash with
        | Ok oldData, Ok newData ->
            let oldText = 
                match oldData with
                | Some d -> System.Text.Encoding.UTF8.GetString d
                | None -> ""
            
            let newText = 
                match newData with
                | Some d -> System.Text.Encoding.UTF8.GetString d
                | None -> ""
            
            let patch = $"--- a/{change.Path}\n+++ b/{change.Path}\n"
            
            Ok patch
        | Error msg, _ -> Error msg
        | _, Error msg -> Error msg
