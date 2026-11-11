namespace Rbt.Git

module Merge =

    /// The clean result is a persisted tree object, never an in-memory hash.
    let mergeCommits (repo: Repo) (baseCommit: GitHash) (ourCommit: GitHash) (theirCommit: GitHash) : Result<MergeResult, string> =
        match ReadObjects.readCommit repo baseCommit, ReadObjects.readCommit repo ourCommit, ReadObjects.readCommit repo theirCommit with
        | Error message, _, _ | _, Error message, _ | _, _, Error message -> Error message
        | Ok baseData, Ok ourData, Ok theirData ->
            Merge3.mergeTree repo baseData.Tree ourData.Tree theirData.Tree

    let canFastForward (repo: Repo) (branch: string) (newCommit: GitHash) : Result<bool, string> =
        match References.resolveReference repo $"refs/heads/{branch}" with
        | Ok currentHash -> CommitHistory.isAncestor repo currentHash newCommit
        | Error message -> Error message

    let fastForward (repo: Repo) (branch: string) (newCommit: GitHash) : Result<unit, string> =
        match canFastForward repo branch newCommit with
        | Ok true -> References.updateBranch repo branch newCommit
        | Ok false -> Error $"Cannot fast-forward branch {branch}"
        | Error message -> Error message

    let findMergeBase (repo: Repo) (commit1: GitHash) (commit2: GitHash) : Result<GitHash, string> =
        match CommitHistory.mergeBase repo commit1 commit2 with
        | Ok (Some hash) -> Ok hash
        | Ok None -> Error "No merge base found"
        | Error message -> Error message

    let threeWayMerge (repo: Repo) (ourCommit: GitHash) (theirCommit: GitHash) : Result<MergeResult, string> =
        match findMergeBase repo ourCommit theirCommit with
        | Ok baseCommit -> mergeCommits repo baseCommit ourCommit theirCommit
        | Error message -> Error message

    let checkConflicts (repo: Repo) (commit1: GitHash) (commit2: GitHash) : Result<GitHash[], string> =
        match threeWayMerge repo commit1 commit2 with
        | Ok (Conflict paths) -> Ok paths
        | Ok (Clean _) -> Ok [||]
        | Error message -> Error message
