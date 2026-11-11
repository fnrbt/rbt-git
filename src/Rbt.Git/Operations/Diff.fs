namespace Rbt.Git

open System
open System.IO
open System.Text
open System.Collections.Generic

module Diff =
    let private compareEntries (oldEntries: TreeEntry[]) (newEntries: TreeEntry[]) =
        let oldMap = oldEntries |> Array.map (fun e -> e.Path, e) |> Map.ofArray
        let newMap = newEntries |> Array.map (fun e -> e.Path, e) |> Map.ofArray
        Set.union (oldMap |> Map.keys |> Set.ofSeq) (newMap |> Map.keys |> Set.ofSeq)
        |> Seq.choose (fun path ->
            match Map.tryFind path oldMap, Map.tryFind path newMap with
            | Some a, Some b when a.Hash = b.Hash && a.Mode = b.Mode -> None
            | Some a, Some b -> Some { Path = path; OldHash = Some a.Hash; NewHash = Some b.Hash; OldMode = Some a.Mode; NewMode = Some b.Mode; ChangeType = ChangeType.Modified }
            | Some a, None -> Some { Path = path; OldHash = Some a.Hash; NewHash = None; OldMode = Some a.Mode; NewMode = None; ChangeType = ChangeType.Deleted }
            | None, Some b -> Some { Path = path; OldHash = None; NewHash = Some b.Hash; OldMode = None; NewMode = Some b.Mode; ChangeType = ChangeType.Added }
            | _ -> None)
        |> Seq.toArray

    let diffTrees (repo: Repo) (oldTree: GitHash) (newTree: GitHash) : Result<FileChange[], string> =
        TreeOperations.listTreeEntriesRecursive repo oldTree
        |> Result.bind (fun a -> TreeOperations.listTreeEntriesRecursive repo newTree |> Result.map (compareEntries a))

    let diffCommits (repo: Repo) (oldCommit: GitHash) (newCommit: GitHash) : Result<FileChange[], string> =
        ReadObjects.readCommit repo oldCommit
        |> Result.bind (fun a -> ReadObjects.readCommit repo newCommit |> Result.bind (fun b -> diffTrees repo a.Tree b.Tree))

    let getChangedFiles (repo: Repo) (commit: GitHash) : Result<FileChange[], string> =
        ReadObjects.readCommit repo commit
        |> Result.bind (fun data ->
            if data.Parents.Length = 0 then
                TreeOperations.listTreeEntriesRecursive repo data.Tree |> Result.map (compareEntries [||])
            else
                ReadObjects.readCommit repo data.Parents.[0] |> Result.bind (fun parent -> diffTrees repo parent.Tree data.Tree))

    let diffWorkingTree (repo: Repo) (tree: GitHash) : Result<FileChange[], string> =
        if not (Repository.hasWorkTree repo) then Error "Cannot diff working tree of bare repository"
        else
            try
                let honorFileMode = TreeOperations.honorsFileMode repo |> Result.defaultWith (fun e -> raise (InvalidDataException e))
                IndexParser.readIndex repo |> Result.bind (fun index ->
                    TreeOperations.listTreeEntriesRecursive repo tree |> Result.bind (fun oldEntries ->
                        let entries = ResizeArray<TreeEntry>()
                        let mutable error = None
                        for entry in index.Entries do
                            if entry.Stage <> 0 then error <- Some $"Unmerged index entry: {entry.Path}"
                            else
                                match TreeOperations.safeWorkTreePath repo entry.Path with
                                | Error msg -> error <- Some msg
                                | Ok path ->
                                    if entry.Mode &&& 0o170000 = 0o160000 then
                                        if Directory.Exists path then entries.Add { Path = entry.Path; Hash = entry.Hash; Mode = entry.Mode }
                                    else
                                        let info = FileInfo path
                                        if not (isNull info.LinkTarget) then
                                            let hash = Hashing.hashBlob (Encoding.UTF8.GetBytes info.LinkTarget)
                                            entries.Add { Path = entry.Path; Hash = hash; Mode = 0o120000 }
                                        elif File.Exists path then
                                            let mode =
                                                if OperatingSystem.IsWindows() || not honorFileMode then entry.Mode
                                                elif File.GetUnixFileMode(path) &&& UnixFileMode.UserExecute <> enum<UnixFileMode>(0) then 0o100755
                                                else 0o100644
                                            entries.Add { Path = entry.Path; Hash = Hashing.hashBlob (File.ReadAllBytes path); Mode = mode }
                        match error with
                        | Some msg -> Error msg
                        | None -> Ok (compareEntries oldEntries (entries.ToArray()))))
            with ex -> Error $"Failed to diff worktree: {ex.Message}"

    let getPatch (repo: Repo) (change: FileChange) : Result<string, string> =
        let load isNew hash mode =
            match hash, mode with
            | None, None -> Ok [||]
            | Some hash, Some mode when mode &&& 0o170000 = 0o160000 ->
                Ok (Encoding.UTF8.GetBytes($"Subproject commit {hash}\n"))
            | Some hash, Some _ ->
                match ReadObjects.readBlob repo hash with
                | Ok bytes -> Ok bytes
                | Error message when isNew && repo.WorkTree.IsSome ->
                    try
                        let path = TreeOperations.safeWorkTreePath repo change.Path |> Result.defaultWith (fun e -> raise (InvalidDataException e))
                        let link = (FileInfo path).LinkTarget
                        let bytes = if isNull link then File.ReadAllBytes path else Encoding.UTF8.GetBytes link
                        if Hashing.hashBlob bytes = hash then Ok bytes else Error message
                    with _ -> Error message
                | Error message -> Error message
            | _ -> Error "Inconsistent patch hash and mode"
        let validUtf8 (bytes: byte[]) =
            try
                UTF8Encoding(false, true).GetCharCount bytes |> ignore
                true
            with :? DecoderFallbackException -> false
        load false change.OldHash change.OldMode |> Result.bind (fun oldBytes ->
            load true change.NewHash change.NewMode |> Result.bind (fun newBytes ->
                if Array.contains 0uy oldBytes || Array.contains 0uy newBytes then Error "Cannot generate a textual patch for binary data"
                elif not (validUtf8 oldBytes && validUtf8 newBytes) then Error "Cannot generate a textual patch for non-UTF-8 data"
                elif change.Path.Contains('\n') || change.Path.Contains('\r') || change.Path.Contains('\t') then Error "Cannot generate a patch for this path"
                else
                    let split (bytes: byte[]) =
                        if bytes.Length = 0 then [||], true
                        else
                            let text = Encoding.UTF8.GetString bytes
                            let ends = text.EndsWith("\n", StringComparison.Ordinal)
                            let lines = text.Split('\n')
                            (if ends then lines.[0 .. lines.Length - 2] else lines), ends
                    let oldLines, oldEnds = split oldBytes
                    let newLines, newEnds = split newBytes
                    let oldName = if change.OldHash.IsNone then "/dev/null" else "a/" + change.Path
                    let newName = if change.NewHash.IsNone then "/dev/null" else "b/" + change.Path
                    let output = StringBuilder()
                    output.Append($"diff --git a/{change.Path} b/{change.Path}\n") |> ignore
                    match change.OldMode, change.NewMode with
                    | None, Some mode -> output.Append(sprintf "new file mode %06o\n" mode) |> ignore
                    | Some mode, None -> output.Append(sprintf "deleted file mode %06o\n" mode) |> ignore
                    | Some oldMode, Some newMode when oldMode <> newMode ->
                        output.Append(sprintf "old mode %06o\nnew mode %06o\n" oldMode newMode) |> ignore
                    | _ -> ()
                    if change.OldHash <> change.NewHash then
                        let oldHash = defaultArg change.OldHash (String('0', 40))
                        let newHash = defaultArg change.NewHash (String('0', 40))
                        let modeSuffix =
                            match change.OldMode, change.NewMode with
                            | Some a, Some b when a = b -> sprintf " %06o" a
                            | _ -> ""
                        output.Append($"index {oldHash}..{newHash}{modeSuffix}\n") |> ignore
                    if oldBytes <> newBytes then
                        output.Append($"--- {oldName}\n+++ {newName}\n") |> ignore
                        let oldStart = if oldLines.Length = 0 then 0 else 1
                        let newStart = if newLines.Length = 0 then 0 else 1
                        output.Append($"@@ -{oldStart},{oldLines.Length} +{newStart},{newLines.Length} @@\n") |> ignore
                        for i = 0 to oldLines.Length - 1 do
                            output.Append('-').Append(oldLines.[i]).Append('\n') |> ignore
                            if i = oldLines.Length - 1 && not oldEnds then output.Append("\\ No newline at end of file\n") |> ignore
                        for i = 0 to newLines.Length - 1 do
                            output.Append('+').Append(newLines.[i]).Append('\n') |> ignore
                            if i = newLines.Length - 1 && not newEnds then output.Append("\\ No newline at end of file\n") |> ignore
                    Ok (output.ToString())))
