namespace Rbt.Git

open System
open System.Collections.Generic
open System.Text

/// Shared three-way tree merge and merge-commit creation.
module Merge3 =

    type MergeOutcome =
        | Merged of GitHash
        | Conflicts of string list
        | MergeError of string

    let private utf8 = UTF8Encoding(false, true)

    let private splitLines (text: string) : string[] =
        let lines = ResizeArray<string>()
        let mutable start = 0
        for i = 0 to text.Length - 1 do
            if text.[i] = '\n' then
                lines.Add(text.Substring(start, i - start + 1))
                start <- i + 1
        if start < text.Length then lines.Add(text.Substring start)
        lines.ToArray()

    /// Hirschberg's LCS: matched index pairs in order, using linear
    /// auxiliary space instead of a quadratic matrix.
    let internal lineMatches (a: string[]) (b: string[]) : (int * int)[] =
        let pairs = ResizeArray<int * int>()
        let row aStart aEnd bStart bEnd backwards =
            let width = bEnd - bStart
            let mutable previous = Array.zeroCreate<int> (width + 1)
            let mutable current = Array.zeroCreate<int> (width + 1)
            for ai = 0 to aEnd - aStart - 1 do
                current.[0] <- 0
                let av = a.[if backwards then aEnd - ai - 1 else aStart + ai]
                for j = 1 to width do
                    let bv = b.[if backwards then bEnd - j else bStart + j - 1]
                    current.[j] <- if av = bv then previous.[j - 1] + 1 else max previous.[j] current.[j - 1]
                let old = previous
                previous <- current
                current <- old
            previous
        let rec visit aStart aEnd bStart bEnd =
            if aStart < aEnd && bStart < bEnd then
                if aEnd - aStart = 1 then
                    let mutable j = bStart
                    while j < bEnd && a.[aStart] <> b.[j] do j <- j + 1
                    if j < bEnd then pairs.Add(aStart, j)
                else
                    let middle = aStart + (aEnd - aStart) / 2
                    let split =
                        let forward = row aStart middle bStart bEnd false
                        let backward = row middle aEnd bStart bEnd true
                        let width = bEnd - bStart
                        let mutable best = -1
                        let mutable choice = 0
                        for j = 0 to width do
                            let score = forward.[j] + backward.[width - j]
                            if score > best then
                                best <- score
                                choice <- j
                        bStart + choice
                    visit aStart middle bStart split
                    visit middle aEnd split bEnd
        visit 0 a.Length 0 b.Length
        pairs.ToArray()

    /// Only valid UTF-8 text is line-merged. Binary and malformed text must
    /// conflict rather than be silently rewritten using replacement characters.
    let mergeText (baseB: byte[]) (oursB: byte[]) (theirsB: byte[]) : Result<byte[], unit> =
        if oursB = theirsB then Ok oursB
        elif baseB = oursB then Ok theirsB
        elif baseB = theirsB then Ok oursB
        elif Array.exists ((=) 0uy) baseB || Array.exists ((=) 0uy) oursB || Array.exists ((=) 0uy) theirsB then Error ()
        else
            try
                let la = splitLines (utf8.GetString baseB)
                let lo = splitLines (utf8.GetString oursB)
                let lt = splitLines (utf8.GetString theirsB)
                let ours = dict (lineMatches la lo)
                let theirs = dict (lineMatches la lt)
                let output = StringBuilder()
                let mutable conflict = false
                let mutable pb = -1
                let mutable po = -1
                let mutable pt = -1
                let apply b o t =
                    let segment (lines: string[]) first last =
                        if first < last then lines.[first .. last - 1] else [||]
                    let original = segment la (pb + 1) b
                    let left = segment lo (po + 1) o
                    let right = segment lt (pt + 1) t
                    if left = right then output.Append(String.Concat left) |> ignore
                    elif left = original then output.Append(String.Concat right) |> ignore
                    elif right = original then output.Append(String.Concat left) |> ignore
                    else conflict <- true
                    if b < la.Length then output.Append(la.[b]) |> ignore
                    pb <- b
                    po <- o
                    pt <- t
                for bi = 0 to la.Length - 1 do
                    match ours.TryGetValue bi, theirs.TryGetValue bi with
                    | (true, oi), (true, ti) -> apply bi oi ti
                    | _ -> ()
                apply la.Length lo.Length lt.Length
                if conflict then Error () else Ok(utf8.GetBytes(output.ToString()))
            with :? DecoderFallbackException -> Error ()

    let private isTree mode = mode = 0o40000
    let private isRegular mode = mode = 0o100644 || mode = 0o100755
    let private isSymlink mode = mode = 0o120000
    let private isGitlink mode = mode = 0o160000

    let private entries (repo: Repo) (hash: GitHash option) : Result<Map<string, TreeEntry>, string> =
        match hash with
        | None -> Ok Map.empty
        | Some tree -> ReadObjects.readTree repo tree |> Result.map (Array.map (fun e -> e.Path, e) >> Map.ofArray)

    // Verify objects retained without rewriting. A gitlink points to a commit
    // in another repository and must never be read as a local blob.
    let rec private verifyEntry (repo: Repo) (entry: TreeEntry) : Result<unit, string> =
        if isGitlink entry.Mode then Ok ()
        elif isTree entry.Mode then
            match ReadObjects.readTree repo entry.Hash with
            | Error message -> Error message
            | Ok children ->
                children
                |> Array.fold (fun result child -> result |> Result.bind (fun () -> verifyEntry repo child)) (Ok ())
        elif isRegular entry.Mode || isSymlink entry.Mode then
            ReadObjects.readBlob repo entry.Hash |> Result.map ignore
        else Error $"Unsupported tree entry mode {entry.Mode} at {entry.Path}"

    let private resolveMode (baseMode: int option) (ours: int) (theirs: int) : int option =
        if ours = theirs then Some ours
        elif baseMode = Some ours then Some theirs
        elif baseMode = Some theirs then Some ours
        else None

    /// Merge recursively; write only fully resolved, verified trees. Errors are
    /// distinct from content conflicts and are never converted to empty objects.
    let rec private mergeLevel (repo: Repo) (prefix: string) (baseT: GitHash option) (ourT: GitHash option) (theirT: GitHash option) : Result<MergeResult, string> =
        match entries repo baseT, entries repo ourT, entries repo theirT with
        | Error message, _, _ | _, Error message, _ | _, _, Error message -> Error message
        | Ok baseEntries, Ok ourEntries, Ok theirEntries ->
            let names =
                Set.unionMany [ baseEntries |> Map.keys |> Set.ofSeq
                                ourEntries |> Map.keys |> Set.ofSeq
                                theirEntries |> Map.keys |> Set.ofSeq ]
            let merged = ResizeArray<TreeEntry>()
            let conflicts = ResizeArray<string>()
            let mutable failure = None
            let keep name (entry: TreeEntry) =
                match verifyEntry repo entry with
                | Ok () -> merged.Add { entry with Path = name }
                | Error message -> failure <- Some message
            for name in names do
                if failure.IsNone then
                    let b = Map.tryFind name baseEntries
                    let o = Map.tryFind name ourEntries
                    let t = Map.tryFind name theirEntries
                    let path = if prefix = "" then name else prefix + "/" + name
                    if o = t then
                        match o with Some e -> keep name e | None -> ()
                    elif o = b then
                        match t with Some e -> keep name e | None -> ()
                    elif t = b then
                        match o with Some e -> keep name e | None -> ()
                    else
                        match o, t with
                        | Some oe, Some te when isTree oe.Mode && isTree te.Mode && (b.IsNone || (b |> Option.exists (fun e -> isTree e.Mode))) ->
                            let baseHash = b |> Option.map (fun e -> e.Hash)
                            match mergeLevel repo path baseHash (Some oe.Hash) (Some te.Hash) with
                            | Ok (Clean hash) -> merged.Add { Mode = 0o40000; Path = name; Hash = hash }
                            | Ok (Conflict paths) -> conflicts.AddRange paths
                            | Error message -> failure <- Some message
                        | Some oe, Some te when isRegular oe.Mode && isRegular te.Mode && (b.IsNone || (b |> Option.exists (fun e -> isRegular e.Mode))) ->
                            match resolveMode (b |> Option.map (fun e -> e.Mode)) oe.Mode te.Mode with
                            | None -> conflicts.Add path
                            | Some mode ->
                                let baseBlob =
                                    match b with None -> Ok [||] | Some e -> ReadObjects.readBlob repo e.Hash
                                match baseBlob, ReadObjects.readBlob repo oe.Hash, ReadObjects.readBlob repo te.Hash with
                                | Error message, _, _ | _, Error message, _ | _, _, Error message -> failure <- Some message
                                | Ok baseBytes, Ok ourBytes, Ok theirBytes ->
                                    match mergeText baseBytes ourBytes theirBytes with
                                    | Error () -> conflicts.Add path
                                    | Ok bytes ->
                                        match ObjectWriter.writeBlob repo bytes with
                                        | Ok hash -> merged.Add { Mode = mode; Path = name; Hash = hash }
                                        | Error message -> failure <- Some message
                        | _ -> conflicts.Add path // symlink, gitlink, modify/delete or type divergence
            match failure with
            | Some message -> Error message
            | None when conflicts.Count > 0 -> Ok (Conflict (conflicts.ToArray()))
            | None -> ObjectWriter.writeTree repo (merged.ToArray()) |> Result.map Clean

    /// Explicit-base merge, shared by both public merge APIs.
    let mergeTree (repo: Repo) (baseTree: GitHash) (ourTree: GitHash) (theirTree: GitHash) : Result<MergeResult, string> =
        mergeLevel repo "" (Some baseTree) (Some ourTree) (Some theirTree)

    /// Merge `theirCommit` into `ourCommit` and persist the resulting merge commit.
    let merge (repo: Repo) (ourCommit: GitHash) (theirCommit: GitHash) (author: Signature) (message: string) : MergeOutcome =
        match CommitHistory.mergeBase repo ourCommit theirCommit with
        | Error error -> MergeError error
        | Ok None -> MergeError "No merge base found"
        | Ok (Some baseHash) ->
            match ReadObjects.readCommit repo baseHash, ReadObjects.readCommit repo ourCommit, ReadObjects.readCommit repo theirCommit with
            | Error error, _, _ | _, Error error, _ | _, _, Error error -> MergeError error
            | Ok baseCommit, Ok ourCommitData, Ok theirCommitData ->
                match mergeTree repo baseCommit.Tree ourCommitData.Tree theirCommitData.Tree with
                | Error error -> MergeError error
                | Ok (Conflict paths) -> Conflicts (List.ofArray paths)
                | Ok (Clean treeHash) ->
                    let commit =
                        { Tree = treeHash
                          Parents = [| ourCommit; theirCommit |]
                          Author = author
                          Committer = author
                          Message = message }
                    match ObjectWriter.writeCommit repo commit with
                    | Ok hash -> Merged hash
                    | Error error -> MergeError error
