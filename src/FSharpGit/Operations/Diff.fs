namespace FSharpGit

open System.Collections.Generic

module Diff =
    
    let private mapTree (repo: Repo) (tree: GitHash) : Result<Dictionary<string, GitHash>, string> =
        TreeOperations.treeToMap repo tree
    
    let rec private diffTreesInternal (entries1: TreeEntry[]) (entries2: TreeEntry[]) (path: string) (repo: Repo) (changes: ResizeArray<FileChange>) : Result<ResizeArray<FileChange>, string> =
        let map1 = entries1 |> Array.map (fun e -> (e.Path, e)) |> dict
        let map2 = entries2 |> Array.map (fun e -> (e.Path, e)) |> dict
        
        let allPaths = 
            [| entries1 |> Array.map (fun e -> e.Path)
               entries2 |> Array.map (fun e -> e.Path) |]
            |> Array.concat
            |> Array.distinct
        
        for path in allPaths do
            let exists1 = map1.ContainsKey path
            let exists2 = map2.ContainsKey path
            
            if exists1 && exists2 then
                let entry1 = map1.[path]
                let entry2 = map2.[path]
                
                if entry1.Hash <> entry2.Hash then
                    let changeType = 
                        if entry1.Mode <> entry2.Mode then
                            Modified
                        else
                            Modified
                    
                    changes.Add {
                        Path = path
                        OldHash = Some entry1.Hash
                        NewHash = Some entry2.Hash
                        ChangeType = changeType
                    }
                
                if entry1.Mode &&& 0o40000 <> 0 || entry2.Mode &&& 0o40000 <> 0 then
                    match TreeOperations.listTree repo entry1.Hash, TreeOperations.listTree repo entry2.Hash with
                    | Ok tree1, Ok tree2 ->
                        let newPath = if System.String.IsNullOrEmpty path then path else $"{path}/"
                        let fullEntries1 = tree1 |> Array.map (fun (e: TreeEntry) -> { e with Path = $"{newPath}{e.Path}" })
                        let fullEntries2 = tree2 |> Array.map (fun (e: TreeEntry) -> { e with Path = $"{newPath}{e.Path}" })
                        match diffTreesInternal fullEntries1 fullEntries2 newPath repo changes with
                        | Ok _ -> ()
                        | Error msg -> Error msg |> ignore
                    | Error msg, _ -> Error msg |> ignore
                    | _, Error msg -> Error msg |> ignore
            elif exists1 && not exists2 then
                let entry1 = map1.[path]
                
                if entry1.Mode &&& 0o40000 <> 0 then
                    match TreeOperations.listTree repo entry1.Hash with
                    | Ok tree1 ->
                        let newPath = if System.String.IsNullOrEmpty path then path else $"{path}/"
                        let fullEntries1 = tree1 |> Array.map (fun (e: TreeEntry) -> { e with Path = $"{newPath}{e.Path}" })
                        match diffTreesInternal fullEntries1 [||] newPath repo changes with
                        | Ok _ -> ()
                        | Error msg -> Error msg |> ignore
                    | Error msg -> Error msg |> ignore
                else
                    changes.Add {
                        Path = path
                        OldHash = Some entry1.Hash
                        NewHash = None
                        ChangeType = Deleted
                    }
            elif not exists1 && exists2 then
                let entry2 = map2.[path]
                
                if entry2.Mode &&& 0o40000 <> 0 then
                    match TreeOperations.listTree repo entry2.Hash with
                    | Ok tree2 ->
                        let newPath = if System.String.IsNullOrEmpty path then path else $"{path}/"
                        let fullEntries2 = tree2 |> Array.map (fun (e: TreeEntry) -> { e with Path = $"{newPath}{e.Path}" })
                        match diffTreesInternal [||] fullEntries2 newPath repo changes with
                        | Ok _ -> ()
                        | Error msg -> Error msg |> ignore
                    | Error msg -> Error msg |> ignore
                else
                    changes.Add {
                        Path = path
                        OldHash = None
                        NewHash = Some entry2.Hash
                        ChangeType = Added
                    }
        
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
