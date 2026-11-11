module HistoryRegression

open System
open System.IO
open System.Text
open Rbt.Git

let private check condition message =
    if not condition then failwith message

let private value = function
    | Ok result -> result
    | Error error -> failwith error

let private signature =
    { Name = "History regression"
      Email = "history@example.invalid"
      Time = DateTimeOffset.FromUnixTimeSeconds(10L) }

let private withRepo test =
    let path = Path.Combine(Path.GetTempPath(), "rbt-history-" + Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory path |> ignore
    try test (Repository.initBare path |> value)
    finally Directory.Delete(path, true)

let private blob repo bytes = ObjectWriter.writeBlob repo bytes |> value
let private text repo (content: string) = blob repo (Encoding.UTF8.GetBytes content)
let private tree repo entries = ObjectWriter.writeTree repo entries |> value
let private entry mode name hash = { Mode = mode; Path = name; Hash = hash }
let private commit repo treeHash parents message =
    ObjectWriter.writeCommit repo
        { Tree = treeHash
          Parents = parents
          Author = signature
          Committer = signature
          Message = message }
    |> value

let private clean = function
    | Ok (Clean hash) -> hash
    | Ok (Conflict paths) -> failwith (sprintf "Unexpected conflicts: %s" (String.concat ", " paths))
    | Error message -> failwith message

let private missingTreeAndReadFailures () =
    withRepo (fun repo ->
        let rootTree = tree repo [||]
        let root = commit repo rootTree [||] "root"
        let invalidTree = String.replicate 40 "a"
        let broken = commit repo invalidTree [| root |] "missing tree"
        match Merge.mergeCommits repo root broken root with
        | Error _ -> ()
        | _ -> failwith "Merge silently accepted a missing tree"
        match Merge3.merge repo broken root signature "merge" with
        | Merge3.MergeError _ -> ()
        | _ -> failwith "Merge3 silently accepted a missing tree"
        let nestedTree = tree repo [| entry 0o40000 "missing" invalidTree |]
        let nested = commit repo nestedTree [| root |] "missing nested tree"
        match Merge.mergeCommits repo root nested nested with
        | Error _ -> ()
        | _ -> failwith "Unchanged missing nested tree was accepted"
        let missingParent = String.replicate 40 "b"
        let damaged = commit repo rootTree [| missingParent |] "missing parent"
        let first = CommitHistory.walkCommits repo damaged |> Seq.truncate 1 |> Seq.toArray
        check (first.Length = 1 && snd first.[0] = damaged) "Bounded walk read unrequested historical commits"
        let proof = commit repo rootTree [| missingParent; root |] "second parent is proof"
        check (CommitHistory.isAncestor repo root proof = Ok true)
            "Known ancestor was rejected because another parent was unreadable"
        match CommitHistory.walkCommitsResult repo damaged with
        | Error _ -> ()
        | _ -> failwith "Walk swallowed an unreadable parent"
        match CommitHistory.isAncestor repo root damaged with
        | Error _ -> ()
        | _ -> failwith "Ancestry swallowed an unreadable parent"
        match CommitHistory.mergeBase repo root damaged with
        | Error _ -> ()
        | _ -> failwith "Merge base swallowed an unreadable parent"
        match CommitHistory.getCommitsBetween repo root damaged with
        | Error _ -> ()
        | _ -> failwith "Commit range swallowed an unreadable parent")

let private dagBasesAndIdentities () =
    withRepo (fun repo ->
        let empty = tree repo [||]
        let root = commit repo empty [||] "root"
        let rawCommit =
            $"tree {empty}\nauthor History <history@example.invalid> 10 +0000\ncommitter History <history@example.invalid> 10 +0000\nencoding UTF-8\n\nraw identity"
            |> Encoding.UTF8.GetBytes
            |> ObjectWriter.writeRaw repo "commit"
            |> value
        let rawChild = commit repo empty [| rawCommit |] "after raw"
        check (CommitHistory.getCommitsBetween repo rawCommit rawChild = Ok [| rawChild |])
            "Noncanonical commit was rehashed instead of retaining its object identity"
        let a = commit repo empty [| root |] "a"
        let b = commit repo empty [| root |] "b"
        let diamond = commit repo empty [| a; b |] "diamond"
        let tip = commit repo empty [| diamond; b |] "tip"
        let ids = CommitHistory.getRevList repo tip
        check (ids.Length = 5 && Set.ofArray ids = Set.ofList [ root; a; b; diamond; tip ]) "Duplicate ancestors truncated graph walk"
        check (CommitHistory.mergeBase repo tip b = Ok (Some b)) "Repeated ancestor had wrong merge base"
        check (CommitHistory.getCommitsBetween repo root tip = Ok [| tip; diamond; b; a |] ||
               (CommitHistory.getCommitsBetween repo root tip |> value |> Set.ofArray) = Set.ofList [ tip; diamond; a; b ])
            "Commit range lost original object identities"
        // The first intersection on a BFS from theirs is root, but the best
        // common ancestor is the edited commit. Ours then reverts that edit.
        let before = text repo "before\n"
        let edited = text repo "edited\n"
        let beforeTree = tree repo [| entry 0o100644 "revert" before |]
        let editedTree = tree repo [| entry 0o100644 "revert" edited |]
        let ancestor = commit repo beforeTree [| root |] "revert root"
        let changedAncestor = commit repo editedTree [| ancestor |] "changed"
        let ours = commit repo beforeTree [| changedAncestor |] "reverted"
        let theirIntermediate = commit repo editedTree [| changedAncestor |] "their intermediate"
        let theirs = commit repo editedTree [| ancestor; theirIntermediate |] "theirs"
        check (CommitHistory.mergeBase repo ours theirs = Ok (Some changedAncestor)) "First BFS intersection was not a lowest common ancestor"
        let result = Merge.threeWayMerge repo ours theirs |> clean |> ReadObjects.readTree repo |> value
        check (result.[0].Hash = before) "Revert was undone by an incorrectly chosen merge base"
        let c1 = commit repo empty [| a; b |] "cross 1"
        let c2 = commit repo empty [| b; a |] "cross 2"
        let bases = CommitHistory.lowestMergeBases repo c1 c2 |> value |> Set.ofArray
        check (bases = Set.ofList [ a; b ]) "Criss-cross best bases not identified"
        match Merge.threeWayMerge repo c1 c2 with
        | Error _ -> ()
        | _ -> failwith "Ambiguous criss-cross merge base silently selected")

let private mergesModesContentAndStructure () =
    withRepo (fun repo ->
        let original = text repo "alpha\nbeta\ngamma\n"
        let changed = text repo "alpha\nBETA\ngamma\n"
        let baseSub = tree repo [| entry 0o100644 "f" original |]
        let modeSub = tree repo [| entry 0o100755 "f" original |]
        let contentSub = tree repo [| entry 0o100644 "f" changed |]
        let rootBase = tree repo [| entry 0o40000 "dir" baseSub |]
        let rootOurs = tree repo [| entry 0o40000 "dir" modeSub |]
        let rootTheirs = tree repo [| entry 0o40000 "dir" contentSub |]
        let baseCommit = commit repo rootBase [||] "base"
        let ours = commit repo rootOurs [| baseCommit |] "chmod"
        let theirs = commit repo rootTheirs [| baseCommit |] "edit"
        let mergedHash = Merge.mergeCommits repo baseCommit ours theirs |> clean
        let mergedRoot = ReadObjects.readTree repo mergedHash |> value
        let mergedChild = ReadObjects.readTree repo mergedRoot.[0].Hash |> value
        check (mergedChild.Length = 1 && mergedChild.[0].Mode = 0o100755 && mergedChild.[0].Hash = changed)
            "Recursive merge did not combine mode and content independently"
        match Merge3.merge repo ours theirs signature "merged" with
        | Merge3.Merged hash ->
            let mergedCommit = ReadObjects.readCommit repo hash |> value
            check (mergedCommit.Tree = mergedHash && mergedCommit.Parents = [| ours; theirs |]) "Merge3 did not persist correct merge commit"
        | _ -> failwith "Merge3 did not produce clean merge commit"
        // Legacy implementation inverted clean/conflict and returned an
        // unwritten hash. Verify both cases and the persisted clean tree.
        let empty = tree repo [||]
        let emptyCommit = commit repo empty [||] "empty"
        let identical = Merge.mergeCommits repo emptyCommit emptyCommit emptyCommit |> clean
        check (ReadObjects.readTree repo identical = Ok [||]) "Clean empty merge did not persist its tree"
        let changedOurs = text repo "ALPHA\nbeta\ngamma\n"
        let changedTheirs = text repo "alpha\nbeta\nGAMMA\n"
        let changeOursTree = tree repo [| entry 0o100644 "f" changedOurs |]
        let changeTheirsTree = tree repo [| entry 0o100644 "f" changedTheirs |]
        let bothOurs = commit repo (tree repo [| entry 0o40000 "dir" changeOursTree |]) [| baseCommit |] "first line"
        let bothTheirs = commit repo (tree repo [| entry 0o40000 "dir" changeTheirsTree |]) [| baseCommit |] "last line"
        let combined = Merge.mergeCommits repo baseCommit bothOurs bothTheirs |> clean
        let combinedRoot = ReadObjects.readTree repo combined |> value
        let combinedSub = ReadObjects.readTree repo combinedRoot.[0].Hash |> value
        let combinedBytes = ReadObjects.readBlob repo combinedSub.[0].Hash |> value
        check (Encoding.UTF8.GetString combinedBytes = "ALPHA\nbeta\nGAMMA\n") "Nonoverlapping text edits did not merge"
        let deleted = tree repo [||]
        let onlyDeleted = commit repo deleted [| baseCommit |] "delete dir"
        let deletedResult = Merge.mergeCommits repo baseCommit onlyDeleted baseCommit |> clean
        check (ReadObjects.readTree repo deletedResult = Ok [||]) "One-sided deletion was resurrected"
        let separate = tree repo [| entry 0o40000 "dir" contentSub; entry 0o100644 "new" changed |]
        let deletedCommit = commit repo deleted [| baseCommit |] "deleted"
        let changedCommit = commit repo separate [| baseCommit |] "replace"
        match Merge.mergeCommits repo baseCommit deletedCommit changedCommit with
        | Ok (Conflict paths) when Array.contains "dir" paths -> ()
        | _ -> failwith "Delete/modify directory conflict was incorrectly resolved")

let private binarySymlinksAndGitlinks () =
    withRepo (fun repo ->
        let source = blob repo [| 255uy; 10uy |]
        let left = blob repo [| 255uy; 1uy; 10uy |]
        let right = blob repo [| 255uy; 2uy; 10uy |]
        let originalTree = tree repo [| entry 0o100644 "bytes" source |]
        let leftTree = tree repo [| entry 0o100644 "bytes" left |]
        let rightTree = tree repo [| entry 0o100644 "bytes" right |]
        let baseCommit = commit repo originalTree [||] "base"
        let leftCommit = commit repo leftTree [| baseCommit |] "left"
        let rightCommit = commit repo rightTree [| baseCommit |] "right"
        match Merge.mergeCommits repo baseCommit leftCommit rightCommit with
        | Ok (Conflict paths) when paths = [| "bytes" |] -> ()
        | _ -> failwith "Invalid UTF-8 was silently transcoded"
        check (Merge3.mergeText [| 0uy; 10uy |] [| 0uy; 1uy; 10uy |] [| 0uy; 2uy; 10uy |] = Error ())
            "NUL-containing binary edits were treated as mergeable text"
        let linkBase = text repo "old/target"
        let linkLeft = text repo "left/target"
        let linkRight = text repo "right/target"
        let linkTree hash = tree repo [| entry 0o120000 "link" hash |]
        let linkRoot = commit repo (linkTree linkBase) [||] "link base"
        let linkOurs = commit repo (linkTree linkLeft) [| linkRoot |] "link ours"
        let linkTheirs = commit repo (linkTree linkRight) [| linkRoot |] "link theirs"
        match Merge.mergeCommits repo linkRoot linkOurs linkTheirs with
        | Ok (Conflict paths) when paths = [| "link" |] -> ()
        | _ -> failwith "Symlink targets were incorrectly text-merged"
        let keptLink = Merge.mergeCommits repo linkRoot linkRoot linkOurs |> clean
        check ((ReadObjects.readTree repo keptLink |> value).[0].Hash = linkLeft)
            "One-sided symlink change did not retain the exact target blob"
        let gitlinkTree hash = tree repo [| entry 0o160000 "submodule" hash |]
        let original = commit repo (gitlinkTree (String.replicate 40 "1")) [||] "gitlink base"
        let ours = commit repo (gitlinkTree (String.replicate 40 "2")) [| original |] "gitlink ours"
        let theirs = commit repo (gitlinkTree (String.replicate 40 "3")) [| original |] "gitlink theirs"
        match Merge.mergeCommits repo original ours theirs with
        | Ok (Conflict paths) when paths = [| "submodule" |] -> ()
        | _ -> failwith "Divergent gitlinks were incorrectly blob-merged"
        let unchanged = Merge.mergeCommits repo original original ours |> clean
        check ((ReadObjects.readTree repo unchanged |> value).[0].Hash = String.replicate 40 "2")
            "Gitlink references were treated as missing local blobs")

let run () =
    missingTreeAndReadFailures ()
    dagBasesAndIdentities ()
    mergesModesContentAndStructure ()
    binarySymlinksAndGitlinks ()
