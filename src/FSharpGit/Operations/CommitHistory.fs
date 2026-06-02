namespace FSharpGit

open System.Collections.Generic

module CommitHistory =
    
    let getParents (repo: Repo) (commit: GitHash) : Result<GitHash[], string> =
        match ReadObjects.readCommit repo commit with
        | Ok commitData -> Ok commitData.Parents
        | Error msg -> Error msg
    
    let rec private walkCommitsInternal (repo: Repo) (visited: HashSet<GitHash>) (queue: ResizeArray<GitHash>) : seq<CommitData * GitHash> =
        seq {
            if queue.Count > 0 then
                let current = queue.[0]
                queue.RemoveAt(0) |> ignore
                
                if not (visited.Contains current) then
                    visited.Add current |> ignore
                    
                    match ReadObjects.readCommit repo current with
                    | Ok commit ->
                        yield (commit, current)
                        
                        for parent in commit.Parents do
                            if not (visited.Contains parent) then
                                queue.Add parent
                        
                        yield! walkCommitsInternal repo visited queue
                    | _ -> ()
        }
    
    let walkCommits (repo: Repo) (start: GitHash) : seq<CommitData * GitHash> =
        let visited = HashSet()
        let queue = ResizeArray()
        queue.Add start
        walkCommitsInternal repo visited queue
    
    let rec private isAncestorInternal (repo: Repo) (ancestor: GitHash) (descendant: GitHash) (visited: HashSet<GitHash>) : Result<bool, string> =
        if ancestor = descendant then
            Ok true
        elif visited.Contains descendant then
            Ok false
        else
            visited.Add descendant |> ignore
            match getParents repo descendant with
            | Ok parents ->
                if Array.isEmpty parents then
                    Ok false
                else
                    let results = parents |> Array.map (fun p -> isAncestorInternal repo ancestor p visited)
                    match results |> Array.tryFind (fun r -> match r with Ok true -> true | _ -> false) with
                    | Some (Ok true) -> Ok true
                    | _ -> Ok false
            | Error msg -> Error msg
    
    let isAncestor (repo: Repo) (ancestor: GitHash) (descendant: GitHash) : Result<bool, string> =
        if ancestor = descendant then
            Ok true
        else
            isAncestorInternal repo ancestor descendant (HashSet())
    
    let rec private collectAncestors (repo: Repo) (commit: GitHash) (visited: HashSet<GitHash>) : Result<HashSet<GitHash>, string> =
        if visited.Contains commit then
            Ok visited
        else
            visited.Add commit |> ignore
            match getParents repo commit with
            | Ok parents ->
                match parents |> Array.fold (fun acc p ->
                    match acc with
                    | Ok v -> collectAncestors repo p v
                    | Error e -> Error e) (Ok visited) with
                | Ok v -> Ok v
                | Error msg -> Error msg
            | Error msg -> Error msg
    
    let rec private mergeBaseInternal (repo: Repo) (commits1: ResizeArray<GitHash>) (commits2: ResizeArray<GitHash>) (candidates: HashSet<GitHash * bool * bool>) : Result<GitHash option, string> =
        if commits1.Count = 0 && commits2.Count = 0 then
            Ok None
        else
            let processCommits (commits: ResizeArray<GitHash>) (isFirst: bool) : Result<unit, string> =
                if commits.Count > 0 then
                    let current = commits.[0]
                    commits.RemoveAt(0) |> ignore
                    
                    match candidates |> Seq.tryFind (fun (c, _, _) -> c = current) with
                    | Some (c, b1, b2) ->
                        if isFirst && b2 then Ok ()
                        elif not isFirst && b1 then Ok ()
                        else
                            candidates.Remove (c, b1, b2) |> ignore
                            let newB1 = if isFirst then true else b1
                            let newB2 = if not isFirst then true else b2
                            candidates.Add (c, newB1, newB2) |> ignore
                            Ok ()
                    | None ->
                        candidates.Add (current, isFirst, false) |> ignore
                        match getParents repo current with
                        | Ok parents -> parents |> Array.iter commits.Add; Ok ()
                        | Error msg -> Error msg
                else
                    Ok ()
            
            match processCommits commits1 true with
            | Ok () -> 
                match processCommits commits2 false with
                | Ok () ->
                    match candidates |> Seq.tryFind (fun (_, b1, b2) -> b1 && b2) with
                    | Some (c, _, _) -> Ok (Some c)
                    | None -> mergeBaseInternal repo commits1 commits2 candidates
                | Error msg -> Error msg
            | Error msg -> Error msg
    
    let mergeBase (repo: Repo) (commit1: GitHash) (commit2: GitHash) : Result<GitHash option, string> =
        if commit1 = commit2 then
            Ok (Some commit1)
        else
            let commits1 = ResizeArray()
            let commits2 = ResizeArray()
            commits1.Add commit1
            commits2.Add commit2
            mergeBaseInternal repo commits1 commits2 (HashSet())
    
    let getCommitCount (repo: Repo) (start: GitHash) : int =
        walkCommits repo start |> Seq.length
    
    let getCommonAncestors (repo: Repo) (commit1: GitHash) (commit2: GitHash) : Result<GitHash[], string> =
        match collectAncestors repo commit1 (HashSet()) with
        | Ok ancestors1 ->
            match collectAncestors repo commit2 (HashSet()) with
            | Ok ancestors2 ->
                let common = Set.intersect (Set.ofSeq ancestors1) (Set.ofSeq ancestors2)
                Ok (Set.toArray common)
            | Error msg -> Error msg
        | Error msg -> Error msg
    
    let getRevList (repo: Repo) (start: GitHash) : GitHash[] =
        walkCommits repo start |> Seq.map snd |> Seq.toArray
    
    let getCommitsBetween (repo: Repo) (baseCommit: GitHash) (headCommit: GitHash) : Result<GitHash[], string> =
        match isAncestor repo baseCommit headCommit with
        | Ok true ->
            let allCommits = walkCommits repo headCommit |> Seq.toArray
            let ancestorCommits = walkCommits repo baseCommit |> Seq.map fst |> Seq.map (fun c -> Hashing.hashCommit c) |> Set.ofSeq
            
            let filtered = 
                allCommits 
                |> Array.filter (fun (_, h) -> not (ancestorCommits.Contains h))
            
            Ok (filtered |> Array.map snd)
        | Ok false -> Ok [||]
        | Error msg -> Error msg
