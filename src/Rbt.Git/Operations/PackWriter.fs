namespace Rbt.Git

open System
open System.Collections.Generic
open System.IO
open System.IO.Compression
open System.Security.Cryptography

/// Packfile generation (for serving clone/fetch) and unpacking (for receiving
/// pushes). Phase 1 writes packs with every object stored undeltified — valid
/// and simple; delta compression is a later optimization. Unpacking DOES resolve
/// ref/ofs deltas, since real `git push` may send them.
module PackWriter =

    // git pack object type ids
    let private OBJ_COMMIT = 1
    let private OBJ_TREE = 2
    let private OBJ_BLOB = 3
    let private OBJ_TAG = 4
    let private OBJ_OFS_DELTA = 6
    let private OBJ_REF_DELTA = 7

    let private typeNum (t: string) =
        match t with
        | "commit" -> OBJ_COMMIT
        | "tree" -> OBJ_TREE
        | "blob" -> OBJ_BLOB
        | "tag" -> OBJ_TAG
        | _ -> failwithf "unknown object type: %s" t

    let private typeName (n: int) =
        match n with
        | 1 -> "commit" | 2 -> "tree" | 3 -> "blob" | 4 -> "tag"
        | _ -> failwithf "unknown pack object type: %d" n

    // ---- object closure ----------------------------------------------------

    /// Objects to send for `wants` to a client that has `haves` and everything
    /// reachable from them, where `clientShallow` lists the client's shallow
    /// commits (their parents are not on the client).
    ///
    /// Commits are walked newest first from both sides together, and the walk
    /// ends once every queued commit is one the client has. Each new commit's
    /// tree is then compared path by path with its parents' trees, and only
    /// what differs is read further. The cost therefore follows the new
    /// history, not the repository: walking every object reachable from the
    /// haves (0.2.0) made an incremental fetch of a large repository read its
    /// whole history first, which ran for minutes with no output.
    ///
    /// Clock skew can make the walk include commits the client already has;
    /// that only enlarges the pack. Nothing the client lacks is excluded.
    let objectClosureFor (repo: Repo) (wants: GitHash[]) (haves: GitHash[]) (clientShallow: seq<GitHash>) : GitHash[] =
        let shallow = HashSet<GitHash>(clientShallow, StringComparer.Ordinal)
        use session = new PackStore.ReadSession(repo)
        let readObject hash = ReadObjects.readObjectWith session repo hash
        let read (hash: GitHash) (expected: string option) =
            match readObject hash with
            | Error error -> failwithf "reading %s: %s" hash error
            | Ok value ->
                let actual = ObjectWriter.objectTypeName value
                match expected with
                | Some t when t <> actual -> failwithf "object %s: expected %s, found %s" hash t actual
                | _ -> value
        let commits = Dictionary<GitHash, CommitData>(StringComparer.Ordinal)
        let commitOf (hash: GitHash) =
            match commits.TryGetValue hash with
            | true, commit -> commit
            | _ ->
                match read hash (Some "commit") with
                | Commit commit ->
                    commits.[hash] <- commit
                    commit
                | _ -> failwithf "object %s is not a commit" hash

        let included = HashSet<GitHash>(StringComparer.Ordinal)
        let wantCommits = ResizeArray<GitHash>()
        let wantTrees = ResizeArray<GitHash>()
        let rec peelWant (hash: GitHash) (expected: string option) =
            match read hash expected with
            | Tag tag ->
                included.Add hash |> ignore
                peelWant tag.Object (Some tag.ObjectType)
            | Commit commit ->
                commits.[hash] <- commit
                wantCommits.Add hash
            | Tree _ -> wantTrees.Add hash
            | Blob _ -> included.Add hash |> ignore
        for want in wants do peelWant want None

        // A have the client claims but this repository lacks, or one that is
        // not a commit, says nothing about history; it is skipped.
        let rec peelHave (hash: GitHash) depth =
            if depth > 16 then None
            else
                match readObject hash with
                | Ok (Commit commit) ->
                    commits.[hash] <- commit
                    Some hash
                | Ok (Tag tag) -> peelHave tag.Object (depth + 1)
                | _ -> None

        let uninteresting = HashSet<GitHash>(StringComparer.Ordinal)
        let interesting = HashSet<GitHash>(StringComparer.Ordinal)
        let propagated = HashSet<GitHash>(StringComparer.Ordinal)
        let queue = PriorityQueue<GitHash, struct (int64 * int64)>()
        let mutable sequence = 0L
        let enqueue (hash: GitHash) (commit: CommitData) =
            sequence <- sequence + 1L
            queue.Enqueue(hash, struct (-commit.Committer.Time.ToUnixTimeSeconds(), sequence))
        for have in haves do
            match peelHave have 0 with
            | Some hash when uninteresting.Add hash -> enqueue hash commits.[hash]
            | _ -> ()
        for hash in wantCommits do
            if not (uninteresting.Contains hash) && interesting.Add hash then enqueue hash commits.[hash]

        let anyInterestingQueued () =
            queue.UnorderedItems |> Seq.exists (fun struct (hash, _) -> not (uninteresting.Contains hash))
        let newCommits = ResizeArray<GitHash>()
        while queue.Count > 0 && anyInterestingQueued () do
            let hash = queue.Dequeue()
            if uninteresting.Contains hash then
                if propagated.Add hash && not (shallow.Contains hash) then
                    for parent in commits.[hash].Parents do
                        // History the server lacks below a common commit is the
                        // client's own business, not something to send.
                        match peelHave parent 0 with
                        | Some parentHash when uninteresting.Add parentHash -> enqueue parentHash commits.[parentHash]
                        | _ -> ()
            else
                newCommits.Add hash
                for parent in (commitOf hash).Parents do
                    if not (uninteresting.Contains parent) && interesting.Add parent then
                        enqueue parent (commitOf parent)

        // Parents before children, so every base tree below is already covered:
        // by the client, or by this pack. A tree reached a second time is then
        // skipped safely, because its first walk compared it with covered bases.
        let newSet = HashSet<GitHash>(newCommits |> Seq.filter (uninteresting.Contains >> not), StringComparer.Ordinal)
        let newCommits =
            let ordered = ResizeArray<GitHash>()
            let visited = HashSet<GitHash>(StringComparer.Ordinal)
            let stack = Stack<struct (GitHash * bool)>()
            for start in newSet do
                stack.Push(struct (start, false))
                while stack.Count > 0 do
                    let struct (hash, expanded) = stack.Pop()
                    if expanded then ordered.Add hash
                    elif visited.Add hash then
                        stack.Push(struct (hash, true))
                        for parent in commits.[hash].Parents do
                            if newSet.Contains parent && not (visited.Contains parent) then stack.Push(struct (parent, false))
            ordered.ToArray()
        // A parent the client has covers its own tree; a new parent's tree is in
        // the pack by the same rule, applied to that parent first.
        // A new commit's tree is read again as its child's base. Reads are not
        // cheap (a large root tree costs tens of milliseconds), so trees are
        // read once; the cache is dropped when it grows large, as in a clone.
        let treeCache = Dictionary<GitHash, TreeEntry[]>(StringComparer.Ordinal)
        let treeEntries (hash: GitHash) =
            match treeCache.TryGetValue hash with
            | true, entries -> entries
            | _ ->
                let entries =
                    match read hash (Some "tree") with
                    | Tree entries -> entries
                    | _ -> [||]
                if treeCache.Count >= 65536 then treeCache.Clear()
                treeCache.[hash] <- entries
                entries
        let rec walkChanged (tree: GitHash) (bases: GitHash list) =
            if not (List.contains tree bases) && included.Add tree then
                let baseEntries =
                    bases |> List.map (fun baseTree ->
                        let byPath = Dictionary<string, TreeEntry>(StringComparer.Ordinal)
                        for entry in treeEntries baseTree do byPath.[entry.Path] <- entry
                        byPath)
                let atPath (entry: TreeEntry) =
                    baseEntries |> List.choose (fun byPath ->
                        match byPath.TryGetValue entry.Path with
                        | true, b -> Some b
                        | _ -> None)
                let inBase (entry: TreeEntry) = atPath entry |> List.exists (fun b -> b.Hash = entry.Hash)
                for entry in treeEntries tree do
                    if entry.Mode = 0o040000 then
                        if not (inBase entry) then
                            let subBases = atPath entry |> List.filter (fun b -> b.Mode = 0o040000) |> List.map _.Hash
                            walkChanged entry.Hash subBases
                    elif entry.Mode <> 0o160000 && not (inBase entry) then
                        included.Add entry.Hash |> ignore
        for hash in newCommits do
            included.Add hash |> ignore
            let commit = commits.[hash]
            // Every parent is either one the client has or a new commit in this pack.
            let bases = commit.Parents |> Seq.map (fun parent -> (commitOf parent).Tree) |> List.ofSeq
            walkChanged commit.Tree bases
        for tree in wantTrees do walkChanged tree []
        included |> Seq.toArray

    /// Objects to send for `wants` to a complete (non-shallow) client that has `haves`.
    let objectClosure (repo: Repo) (wants: GitHash[]) (haves: GitHash[]) : GitHash[] =
        objectClosureFor repo wants haves Seq.empty

    /// Depth counts commits, not annotated tags or trees.
    let objectClosureShallow (repo: Repo) (wants: GitHash[]) (depth: int) : GitHash[] * GitHash[] =
        // One pack session for the walk (see objectClosureFor): a session per
        // object read made a depth-1 clone of a large tree take minutes.
        use session = new PackStore.ReadSession(repo)
        let included = HashSet<GitHash>()
        let shallow = HashSet<GitHash>()
        let bestDepth = Dictionary<GitHash, int>()
        let pending = Queue<GitHash * int>()
        for want in wants do pending.Enqueue(want, 1)
        while pending.Count > 0 do
            let hash, d = pending.Dequeue()
            let shouldVisit =
                match bestDepth.TryGetValue hash with
                | true, old when old <= d -> false
                | _ -> bestDepth.[hash] <- d; true
            if shouldVisit then
                included.Add hash |> ignore
                match ReadObjects.readObjectWith session repo hash with
                | Error error -> failwithf "reading %s: %s" hash error
                | Ok (Commit commit) ->
                    pending.Enqueue(commit.Tree, d)
                    if d >= depth then
                        if commit.Parents.Length > 0 then shallow.Add hash |> ignore
                    else
                        shallow.Remove hash |> ignore
                        for parent in commit.Parents do pending.Enqueue(parent, d + 1)
                | Ok (Tree entries) ->
                    for entry in entries do
                        if entry.Mode = 0o040000 then pending.Enqueue(entry.Hash, d)
                        elif entry.Mode <> 0o160000 then included.Add entry.Hash |> ignore
                | Ok (Tag tag) ->
                    if tag.ObjectType = "blob" then included.Add tag.Object |> ignore
                    else pending.Enqueue(tag.Object, d)
                | Ok (Blob _) -> ()
        included |> Seq.toArray, shallow |> Seq.toArray

    // ---- pack writing ------------------------------------------------------

    let private writeObjHeader (ms: MemoryStream) (typeN: int) (size: int) =
        let mutable b = (typeN <<< 4) &&& 0x70
        b <- b ||| (size &&& 0x0F)
        let mutable s = size >>> 4
        if s > 0 then b <- b ||| 0x80
        ms.WriteByte(byte b)
        while s > 0 do
            let mutable c = s &&& 0x7F
            s <- s >>> 7
            if s > 0 then c <- c ||| 0x80
            ms.WriteByte(byte c)

    let private be32 (n: int) : byte[] =
        [| byte (n >>> 24); byte (n >>> 16); byte (n >>> 8); byte n |]

    /// Build a packfile containing exactly the given object ids.
    let writePackFor (repo: Repo) (objectIds: GitHash[]) : Result<byte[], string> =
        try
            use ms = new MemoryStream()
            ms.Write(Text.Encoding.ASCII.GetBytes "PACK", 0, 4)
            ms.Write(be32 2, 0, 4)
            ms.Write(be32 objectIds.Length, 0, 4)
            for h in objectIds do
                match ReadObjects.readRawObject repo h with
                | Ok (t, content) ->
                    writeObjHeader ms (typeNum t) content.Length
                    let compressed = Compression.compress content
                    ms.Write(compressed, 0, compressed.Length)
                | Error e -> failwithf "reading %s: %s" h e
            let body = ms.ToArray()
            let checksum = SHA1.HashData body
            Ok (Array.append body checksum)
        with ex -> Error $"writePack failed: {ex.Message}"

    let writePack (repo: Repo) (wants: GitHash[]) (haves: GitHash[]) : Result<byte[], string> =
        try writePackFor repo (objectClosure repo wants haves)
        with ex -> Error $"writePack failed: {ex.Message}"

    /// Like writePack, but delta-compresses the pack (ofs-delta) when useDelta is set.
    let writePackOpt (repo: Repo) (wants: GitHash[]) (haves: GitHash[]) (useDelta: bool) : Result<byte[], string> =
        try Ok(fst (Gc.buildPack repo (objectClosure repo wants haves) useDelta))
        with ex -> Error $"writePack failed: {ex.Message}"

    // ---- pack unpacking (receive) -----------------------------------------
    type private PendingEntry =
        | Whole of string * string
        | OffsetDelta of int * string
        | ReferenceDelta of GitHash * string


    /// Stream-unpack a received packfile into `targetRepo`, resolving thin-pack
    /// bases from `sourceRepo`. The complete pack trailer is verified before
    /// success is returned.
    let unpackPackStreamInto
        (sourceRepo: Repo)
        (targetRepo: Repo)
        (counting: PackData.CountingStream) : Result<GitHash[], string> =
        try
            let magic = PackData.readExact counting 4
            if not (magic.[0] = byte 'P' && magic.[1] = byte 'A' && magic.[2] = byte 'C' && magic.[3] = byte 'K') then
                Error "not a packfile"
            else
                let versionBytes = PackData.readExact counting 4
                let version =
                    (int versionBytes.[0] <<< 24)
                    ||| (int versionBytes.[1] <<< 16)
                    ||| (int versionBytes.[2] <<< 8)
                    ||| int versionBytes.[3]
                if version <> 2 && version <> 3 then
                    Error $"unsupported pack version {version}"
                else
                    let cb = PackData.readExact counting 4
                    let count = (int cb.[0] <<< 24) ||| (int cb.[1] <<< 16) ||| (int cb.[2] <<< 8) ||| int cb.[3]
                    if count < 0 then failwith "invalid pack object count"
                    let spoolDir = Path.Combine(Repository.getObjectsDir targetRepo, "tmp-unpack-" + Guid.NewGuid().ToString("N"))
                    Directory.CreateDirectory spoolDir |> ignore
                    use spool = { new IDisposable with member _.Dispose() = Directory.Delete(spoolDir, true) }
                    let stage offset (content: byte[]) =
                        let path = Path.Combine(spoolDir, string offset)
                        File.WriteAllBytes(path, content)
                        path
                    let entries = ResizeArray<int * PendingEntry>()
                    for _ in 1 .. count do
                        let entryStart = counting.Count
                        let typeId, size = PackData.readObjHeaderFromStream counting
                        let entry =
                            match typeId with
                            | 1 | 2 | 3 | 4 ->
                                Whole(PackData.typeName typeId, stage entryStart (PackData.inflateFromStream counting size))
                            | 6 ->
                                let rel = PackData.readOfsBaseFromStream counting
                                OffsetDelta(entryStart - rel, stage entryStart (PackData.inflateFromStream counting size))
                            | 7 ->
                                let baseHash = Convert.ToHexString(PackData.readExact counting 20).ToLowerInvariant()
                                ReferenceDelta(baseHash, stage entryStart (PackData.inflateFromStream counting size))
                            | other -> failwithf "unsupported pack object type %d" other
                        entries.Add(entryStart, entry)

                    counting.StopHashing()
                    let trailer = PackData.readExact counting 20
                    let computed = counting.GetHashAndReset()
                    if not (CryptographicOperations.FixedTimeEquals(trailer, computed)) then
                        Error "pack trailer checksum mismatch"
                    else
                        let byOffset = Dictionary<int, GitHash>()
                        let readBase hash =
                            match ReadObjects.readRawObject targetRepo hash with
                            | Ok value -> Some value
                            | Error _ -> ReadObjects.readRawObject sourceRepo hash |> Result.toOption
                        let written = ResizeArray<GitHash>()
                        let mutable remaining = List.ofSeq entries
                        while not remaining.IsEmpty do
                            let deferred = ResizeArray<int * PendingEntry>()
                            let mutable progress = false
                            for offset, entry in remaining do
                                let result =
                                    match entry with
                                    | Whole(t, path) -> Some(t, File.ReadAllBytes path)
                                    | OffsetDelta(baseOffset, path) ->
                                        match byOffset.TryGetValue baseOffset with
                                        | true, baseHash ->
                                            readBase baseHash |> Option.map (fun (t, content) -> t, PackData.applyDelta content (File.ReadAllBytes path))
                                        | _ -> None
                                    | ReferenceDelta(baseHash, path) ->
                                        readBase baseHash |> Option.map (fun (t, content) -> t, PackData.applyDelta content (File.ReadAllBytes path))
                                match result with
                                | None -> deferred.Add(offset, entry)
                                | Some (t, content) ->
                                    let hash =
                                        match ObjectWriter.writeRaw targetRepo t content with
                                        | Ok hash -> hash
                                        | Error error -> failwith error
                                    byOffset.[offset] <- hash
                                    written.Add hash
                                    progress <- true
                            if not progress then failwith "unresolved or cyclic delta bases"
                            remaining <- List.ofSeq deferred
                        Ok(written.ToArray())
        with ex -> Error $"unpackPackStream failed: {ex.Message}"

    let unpackPackStream (repo: Repo) (counting: PackData.CountingStream) : Result<GitHash[], string> =
        unpackPackStreamInto repo repo counting

    let unpackPack (repo: Repo) (pack: byte[]) : Result<GitHash[], string> =
        use input = new MemoryStream(pack)
        use counting = new PackData.CountingStream(input)
        match unpackPackStreamInto repo repo counting with
        | Ok _ when input.Position <> input.Length -> Error "trailing data after pack checksum"
        | result -> result

    /// Atomically promote validated loose objects from a quarantine repository.
    /// Existing destination objects are immutable and therefore left in place.
    let promoteLooseObjects (quarantine: Repo) (repo: Repo) (hashes: GitHash seq) : Result<unit, string> =
        try
            for hash in hashes |> Seq.distinct do
                let source = Repository.getLooseObjectPath quarantine hash
                let destination = Repository.getLooseObjectPath repo hash
                if not (File.Exists source) then
                    raise (FileNotFoundException($"Quarantine object is missing: {hash}", source))
                if File.Exists destination then
                    File.Delete source
                else
                    Directory.CreateDirectory(Path.GetDirectoryName destination) |> ignore
                    try File.Move(source, destination, false)
                    with :? IOException when File.Exists destination -> File.Delete source
            Ok ()
        with ex -> Error $"Failed to promote quarantine objects: {ex.Message}"
