namespace FSharpGit

open System.Collections.Generic

module Merge =
    
    let private createMergeTree (repo: Repo) (baseTree: GitHash) (ourTree: GitHash) (theirTree: GitHash) : Result<GitHash option, string> =
        let rec mergeTreesRecursive (baseTree: GitHash option) (ourTree: GitHash option) (theirTree: GitHash option) (path: string) : Result<TreeEntry[] * string array, string> =
            let getEntries (treeOpt: GitHash option) : Result<TreeEntry[], string> =
                match treeOpt with
                | Some tree -> TreeOperations.listTree repo tree
                | None -> Ok [||]
            
            let resolveEntry (entries: TreeEntry[]) (name: string) : TreeEntry option =
                entries |> Array.tryFind (fun e -> e.Path = name)
            
            let allNames = ResizeArray()
            
            match getEntries baseTree, getEntries ourTree, getEntries theirTree with
            | Ok baseEntries, Ok ourEntries, Ok theirEntries ->
                let conflicts = ResizeArray()
                let mergedEntries = ResizeArray()
                
                let names = 
                    [| baseEntries |> Array.map (fun e -> e.Path)
                       ourEntries |> Array.map (fun e -> e.Path)
                       theirEntries |> Array.map (fun e -> e.Path) |]
                    |> Array.concat
                    |> Array.distinct
                
                for name in names do
                    let baseEntry = resolveEntry baseEntries name
                    let ourEntry = resolveEntry ourEntries name
                    let theirEntry = resolveEntry theirEntries name
                    
                    let modeMatches = 
                        match baseEntry, ourEntry, theirEntry with
                        | Some b, Some o, Some t -> b.Mode = o.Mode && o.Mode = t.Mode
                        | _ -> true
                    
                    if modeMatches then
                        match baseEntry, ourEntry, theirEntry with
                        | Some b, Some o, Some t when b.Hash = o.Hash && o.Hash = t.Hash ->
                            mergedEntries.Add o
                        | Some b, Some o, Some t when b.Hash = o.Hash && o.Hash <> t.Hash ->
                            mergedEntries.Add t
                        | Some b, Some o, Some t when b.Hash <> o.Hash && o.Hash = t.Hash ->
                            mergedEntries.Add o
                        | Some b, Some o, Some t when b.Hash = t.Hash && o.Hash <> t.Hash ->
                            mergedEntries.Add o
                        | Some b, Some o, Some t when b.Hash = o.Hash && o.Hash <> t.Hash ->
                            if o.Mode &&& 0o40000 <> 0 then
                                match mergeTreesRecursive (Some b.Hash) (Some o.Hash) (Some t.Hash) name with
                                | Ok (entries, subConflicts) ->
                                    mergedEntries.AddRange entries
                                    conflicts.AddRange subConflicts
                                | Error msg -> Error msg |> ignore
                            else
                                conflicts.Add $"{path}{name}"
                        | None, Some o, Some t when o.Hash = t.Hash ->
                            mergedEntries.Add o
                        | Some b, Some o, None when b.Hash = o.Hash ->
                            mergedEntries.Add o
                        | Some b, None, Some t when b.Hash = t.Hash ->
                            mergedEntries.Add t
                        | Some b, Some o, None when b.Hash <> o.Hash ->
                            if o.Mode &&& 0o40000 <> 0 then
                                match mergeTreesRecursive (Some b.Hash) (Some o.Hash) None name with
                                | Ok (entries, subConflicts) ->
                                    mergedEntries.AddRange entries
                                    conflicts.AddRange subConflicts
                                | Error msg -> Error msg |> ignore
                            else
                                mergedEntries.Add o
                        | Some b, None, Some t when b.Hash <> t.Hash ->
                            if t.Mode &&& 0o40000 <> 0 then
                                match mergeTreesRecursive (Some b.Hash) None (Some t.Hash) name with
                                | Ok (entries, subConflicts) ->
                                    mergedEntries.AddRange entries
                                    conflicts.AddRange subConflicts
                                | Error msg -> Error msg |> ignore
                            else
                                mergedEntries.Add t
                        | None, Some o, None ->
                            mergedEntries.Add o
                        | None, None, Some t ->
                            mergedEntries.Add t
                        | _ ->
                            conflicts.Add $"{path}{name}"
                    else
                        conflicts.Add $"{path}{name}"
                
                Ok (mergedEntries.ToArray(), conflicts.ToArray())
            | Error msg, _, _ -> Error msg
            | _, Error msg, _ -> Error msg
            | _, _, Error msg -> Error msg
        
        let newPath = ""
        match mergeTreesRecursive (Some baseTree) (Some ourTree) (Some theirTree) newPath with
        | Ok (entries, conflicts) ->
            if Array.isEmpty conflicts then
                Ok None
            else
                Ok (Some (Hashing.hashTree entries))
        | Error msg -> Error msg
    
    let mergeCommits (repo: Repo) (baseCommit: GitHash) (ourCommit: GitHash) (theirCommit: GitHash) : Result<MergeResult, string> =
        match ReadObjects.readCommit repo baseCommit, ReadObjects.readCommit repo ourCommit, ReadObjects.readCommit repo theirCommit with
        | Ok baseData, Ok ourData, Ok theirData ->
            match createMergeTree repo baseData.Tree ourData.Tree theirData.Tree with
            | Ok None ->
                Ok (Conflict [||])
            | Ok (Some mergeTree) ->
                Ok (Clean mergeTree)
            | Error msg -> Error msg
        | Error msg, _, _ -> Error msg
        | _, Error msg, _ -> Error msg
        | _, _, Error msg -> Error msg
    
    let canFastForward (repo: Repo) (branch: string) (newCommit: GitHash) : Result<bool, string> =
        match References.resolveReference repo $"refs/heads/{branch}" with
        | Ok currentHash ->
            CommitHistory.isAncestor repo currentHash newCommit
        | Error msg -> Error msg
    
    let fastForward (repo: Repo) (branch: string) (newCommit: GitHash) : Result<unit, string> =
        match canFastForward repo branch newCommit with
        | Ok true ->
            References.updateBranch repo branch newCommit
        | Ok false ->
            Error $"Cannot fast-forward branch {branch}"
        | Error msg -> Error msg
    
    let findMergeBase (repo: Repo) (commit1: GitHash) (commit2: GitHash) : Result<GitHash, string> =
        match CommitHistory.mergeBase repo commit1 commit2 with
        | Ok (Some hash) -> Ok hash
        | Ok None -> Error "No merge base found"
        | Error msg -> Error msg
    
    let threeWayMerge (repo: Repo) (ourCommit: GitHash) (theirCommit: GitHash) : Result<MergeResult, string> =
        match findMergeBase repo ourCommit theirCommit with
        | Ok baseCommit ->
            mergeCommits repo baseCommit ourCommit theirCommit
        | Error msg -> Error msg
    
    let checkConflicts (repo: Repo) (commit1: GitHash) (commit2: GitHash) : Result<GitHash[], string> =
        match threeWayMerge repo commit1 commit2 with
        | Ok (Conflict paths) -> Ok paths
        | Ok (Clean _) -> Ok [||]
        | Error msg -> Error msg
