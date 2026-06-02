namespace FSharpGit

open System
open System.IO
open System.Collections.Generic

module RepositoryOperations =
    
    let getStatus (repo: Repo) : Result<StatusEntry[], string> =
        if not (Repository.hasWorkTree repo) then
            Ok [||]
        else
            match IndexParser.readIndex repo with
            | Ok index ->
                let workTree = repo.WorkTree.Value
                let statusEntries = ResizeArray<StatusEntry>()
                
                match References.readHead repo with
                | Ok (Direct headHash) ->
                    match ReadObjects.readCommit repo headHash with
                    | Ok headCommit ->
                        match TreeOperations.treeToMap repo headCommit.Tree with
                        | Ok headTree ->
                            for indexEntry in index.Entries do
                                let fullPath = Path.Combine(workTree, indexEntry.Path.Replace('/', Path.DirectorySeparatorChar))
                                let inIndex = true
                                let inHead = headTree.ContainsKey indexEntry.Path
                                let fileExists = File.Exists fullPath
                                
                                let indexStatus = 
                                    if not inHead then
                                        FileStatus.Added
                                    elif inHead && headTree.[indexEntry.Path] <> indexEntry.Hash then
                                        FileStatus.Modified
                                    else
                                        FileStatus.Unchanged
                                 
                                let workTreeStatus =
                                    if not fileExists then
                                        FileStatus.Deleted
                                    else
                                        let content = File.ReadAllBytes fullPath
                                        let hash = Hashing.hashBlob content
                                        if hash <> indexEntry.Hash then
                                            FileStatus.Modified
                                        else
                                            FileStatus.Unchanged
                                
                                statusEntries.Add {
                                    Path = indexEntry.Path
                                    IndexStatus = indexStatus
                                    WorkTreeStatus = workTreeStatus
                                }
                            
                            let indexPaths = index.Entries |> Array.map (fun e -> e.Path) |> Set.ofArray
                            for headPath in headTree.Keys do
                                if not (indexPaths.Contains headPath) then
                                    let fullPath = Path.Combine(workTree, headPath.Replace('/', Path.DirectorySeparatorChar))
                                    if File.Exists fullPath then
                                        let content = File.ReadAllBytes fullPath
                                        let hash = Hashing.hashBlob content
                                        statusEntries.Add {
                                            Path = headPath
                                            IndexStatus = FileStatus.Deleted
                                            WorkTreeStatus = FileStatus.Modified
                                        }
                                    else
                                        statusEntries.Add {
                                            Path = headPath
                                            IndexStatus = FileStatus.Deleted
                                            WorkTreeStatus = FileStatus.Unchanged
                                        }
                            
                            Ok (statusEntries.ToArray())
                        | Error msg -> Error msg
                    | Error msg -> Error msg
                | Ok (Symbolic _) -> Ok [||]
                | Error msg -> Error msg
            | Error msg -> Error msg
    
    let checkout (repo: Repo) (refName: string) : Result<unit, string> =
        if not (Repository.hasWorkTree repo) then
            Error "Cannot checkout in bare repository"
        else
            match References.resolveReference repo refName with
            | Ok commitHash ->
                match ReadObjects.readCommit repo commitHash with
                | Ok commitData ->
                    let workTree = repo.WorkTree.Value
                    
                    match TreeOperations.treeToMap repo commitData.Tree with
                    | Ok treeMap ->
                        for kvp in treeMap do
                            let fullPath = Path.Combine(workTree, kvp.Key.Replace('/', Path.DirectorySeparatorChar))
                            let dir = Path.GetDirectoryName fullPath
                            if not (Directory.Exists dir) then
                                Directory.CreateDirectory dir |> ignore
                            
                            match ReadObjects.readBlob repo kvp.Value with
                            | Ok content ->
                                File.WriteAllBytes(fullPath, content)
                            | Error msg -> Error msg |> ignore
                        
                        References.updateHead repo (Symbolic refName) |> ignore
                        Ok ()
                    | Error msg -> Error msg
                | Error msg -> Error msg
            | Error msg -> Error msg
    
    let currentBranch (repo: Repo) : Result<string option, string> =
        References.currentBranch repo
    
    let createBranch (repo: Repo) (name: string) (startPoint: GitHash) : Result<unit, string> =
        References.createBranch repo name startPoint
    
    let deleteBranch (repo: Repo) (name: string) : Result<unit, string> =
        References.deleteBranch repo name
    
    let listBranches (repo: Repo) : Result<string[], string> =
        References.listBranches repo
    
    let listTags (repo: Repo) : Result<string[], string> =
        References.listTags repo
    
    let getHead (repo: Repo) : Result<GitHash, string> =
        match References.readHead repo with
        | Ok (Direct hash) -> Ok hash
        | Ok (Symbolic ref) -> References.resolveReference repo ref
        | Error msg -> Error msg
    
    let getCommit (repo: Repo) (hash: GitHash) : Result<CommitData, string> =
        ReadObjects.readCommit repo hash
    
    let getLog (repo: Repo) (maxCount: int option) : Result<CommitData[], string> =
        match getHead repo with
        | Ok headHash ->
            let commits = CommitHistory.walkCommits repo headHash |> Seq.map fst
            match maxCount with
            | Some count -> Ok (commits |> Seq.take count |> Seq.toArray)
            | None -> Ok (commits |> Seq.toArray)
        | Error msg -> Error msg
    
    let getWorkingTreeChanges (repo: Repo) : Result<StatusEntry[], string> =
        match getStatus repo with
        | Ok entries ->
            let changed = entries |> Array.filter (fun e -> e.WorkTreeStatus <> Unchanged)
            Ok changed
        | Error msg -> Error msg
    
    let getStagedChanges (repo: Repo) : Result<StatusEntry[], string> =
        match getStatus repo with
        | Ok entries ->
            let staged = entries |> Array.filter (fun e -> e.IndexStatus <> Unchanged)
            Ok staged
        | Error msg -> Error msg
    
    let isClean (repo: Repo) : Result<bool, string> =
        match getStatus repo with
        | Ok entries ->
            let hasChanges = entries |> Array.exists (fun e -> e.WorkTreeStatus <> Unchanged || e.IndexStatus <> Unchanged)
            Ok (not hasChanges)
        | Error msg -> Error msg
    
    let getFileContent (repo: Repo) (path: string) : Result<byte[], string> =
        match getHead repo with
        | Ok headHash ->
            match ReadObjects.readCommit repo headHash with
            | Ok commitData ->
                TreeOperations.getFileAtCommit repo commitData.Tree path
            | Error msg -> Error msg
        | Error msg -> Error msg
    
    let listDirectory (repo: Repo) (path: string) : Result<TreeEntry[], string> =
        match getHead repo with
        | Ok headHash ->
            match ReadObjects.readCommit repo headHash with
            | Ok commitData ->
                TreeOperations.getTreeAtPath repo commitData.Tree path
            | Error msg -> Error msg
        | Error msg -> Error msg
    
    let blame (repo: Repo) (path: string) : Result<(GitHash * CommitData)[], string> =
        match getHead repo with
        | Ok headHash ->
            match ReadObjects.readCommit repo headHash with
            | Ok commitData ->
                match TreeOperations.getFileAtCommit repo commitData.Tree path with
                | Ok content ->
                    let lines = System.Text.Encoding.UTF8.GetString(content).Split('\n')
                    let blames = 
                        lines |> Array.map (fun _ -> 
                            let commit = 
                                CommitHistory.walkCommits repo headHash 
                                |> Seq.tryPick (fun (c, h) ->
                                    if TreeOperations.pathExists repo c.Tree path then
                                        Some (h, c)
                                    else
                                        None)
                            match commit with
                            | Some (h, c) -> (h, c)
                            | None -> (headHash, commitData))
                    Ok blames
                | Error msg -> Error msg
            | Error msg -> Error msg
        | Error msg -> Error msg
