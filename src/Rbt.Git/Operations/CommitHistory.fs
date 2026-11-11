namespace Rbt.Git

open System.Collections.Generic

module CommitHistory =
    
    let getParents (repo: Repo) (commit: GitHash) : Result<GitHash[], string> =
        match ReadObjects.readCommit repo commit with
        | Ok commitData -> Ok commitData.Parents
        | Error msg -> Error msg
    
    /// Stream one commit at a time. A bounded caller need not read the whole
    /// repository, but any failed read actually reached must throw.
    let walkCommits (repo: Repo) (start: GitHash) : seq<CommitData * GitHash> =
        seq {
            let visited = HashSet<GitHash>()
            let pending = Queue<GitHash>()
            pending.Enqueue start
            while pending.Count > 0 do
                let hash = pending.Dequeue()
                if visited.Add hash then
                    match ReadObjects.readCommit repo hash with
                    | Error message -> invalidOp message
                    | Ok commit ->
                        yield commit, hash
                        for parent in commit.Parents do
                            if not (visited.Contains parent) then pending.Enqueue parent
        }

    /// Complete walks return errors explicitly, rather than a partial history.
    let walkCommitsResult (repo: Repo) (start: GitHash) : Result<(CommitData * GitHash)[], string> =
        try Ok (walkCommits repo start |> Seq.toArray)
        with :? System.InvalidOperationException as ex -> Error ex.Message

    let private collectGraph (repo: Repo) (start: GitHash) : Result<Dictionary<GitHash, GitHash[]>, string> =
        let graph = Dictionary<GitHash, GitHash[]>()
        let pending = Stack<GitHash>()
        pending.Push start
        let mutable error = None
        while pending.Count > 0 && error.IsNone do
            let hash = pending.Pop()
            if not (graph.ContainsKey hash) then
                match getParents repo hash with
                | Error message -> error <- Some message
                | Ok parents ->
                    graph.Add(hash, parents)
                    for parent in parents do
                        if not (graph.ContainsKey parent) then pending.Push parent
        match error with
        | Some message -> Error message
        | None -> Ok graph

    let isAncestor (repo: Repo) (ancestor: GitHash) (descendant: GitHash) : Result<bool, string> =
        let seen = HashSet<GitHash>()
        let pending = Queue<GitHash>()
        pending.Enqueue descendant
        let mutable error = None
        let mutable found = false
        while pending.Count > 0 && not found do
            let hash = pending.Dequeue()
            if hash = ancestor then found <- true
            elif seen.Add hash then
                match getParents repo hash with
                | Error message -> if error.IsNone then error <- Some message
                | Ok parents ->
                    for parent in parents do
                        if not (seen.Contains parent) then pending.Enqueue parent
        if found then Ok true
        else
            match error with
            | Some message -> Error message
            | None -> Ok false

    let getCommonAncestors (repo: Repo) (commit1: GitHash) (commit2: GitHash) : Result<GitHash[], string> =
        match collectGraph repo commit1, collectGraph repo commit2 with
        | Ok left, Ok right ->
            left.Keys
            |> Seq.filter right.ContainsKey
            |> Seq.sort
            |> Seq.toArray
            |> Ok
        | Error message, _ -> Error message
        | _, Error message -> Error message

    /// Return every best common ancestor. Criss-cross histories can have more
    /// than one; picking an arbitrary one silently produces an incorrect merge.
    let lowestMergeBases (repo: Repo) (commit1: GitHash) (commit2: GitHash) : Result<GitHash[], string> =
        match collectGraph repo commit1, collectGraph repo commit2 with
        | Ok left, Ok right ->
            let common = HashSet<GitHash>(left.Keys |> Seq.filter right.ContainsKey)
            let nonLowest = HashSet<GitHash>()
            let pending = Queue<GitHash>()
            for hash in common do
                for parent in left.[hash] do pending.Enqueue parent
            while pending.Count > 0 do
                let hash = pending.Dequeue()
                if nonLowest.Add hash then
                    for parent in left.[hash] do pending.Enqueue parent
            common
            |> Seq.filter (fun hash -> not (nonLowest.Contains hash))
            |> Seq.sort
            |> Seq.toArray
            |> Ok
        | Error message, _ -> Error message
        | _, Error message -> Error message

    let mergeBase (repo: Repo) (commit1: GitHash) (commit2: GitHash) : Result<GitHash option, string> =
        match lowestMergeBases repo commit1 commit2 with
        | Ok [||] -> Ok None
        | Ok [| hash |] -> Ok (Some hash)
        | Ok bases -> Error (sprintf "Multiple merge bases (%s); recursive merge bases are not supported" (String.concat ", " bases))
        | Error message -> Error message
    
    let getCommitCount (repo: Repo) (start: GitHash) : int =
        walkCommits repo start |> Seq.length
    
    let getRevList (repo: Repo) (start: GitHash) : GitHash[] =
        walkCommits repo start |> Seq.map snd |> Seq.toArray
    
    let getCommitsBetween (repo: Repo) (baseCommit: GitHash) (headCommit: GitHash) : Result<GitHash[], string> =
        match walkCommitsResult repo headCommit with
        | Error message -> Error message
        | Ok headCommits ->
            if not (headCommits |> Array.exists (fun (_, hash) -> hash = baseCommit)) then Ok [||]
            else
                match walkCommitsResult repo baseCommit with
                | Error message -> Error message
                | Ok baseCommits ->
                    let excluded = HashSet<GitHash>(baseCommits |> Array.map snd)
                    headCommits
                    |> Array.choose (fun (_, hash) -> if excluded.Contains hash then None else Some hash)
                    |> Ok
