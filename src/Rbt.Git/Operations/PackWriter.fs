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

    /// Traverse every object edge, stopping at known common commits.
    let objectClosure (repo: Repo) (wants: GitHash[]) (haves: GitHash[]) : GitHash[] =
        let excluded = HashSet<GitHash>()
        let walk (seen: HashSet<GitHash>) (stops: HashSet<GitHash>) (roots: GitHash[]) =
            let examined = HashSet<GitHash>()
            let pending = Queue<GitHash * string option>()
            for root in roots do pending.Enqueue(root, None)
            let enqueue hash expected =
                if expected = Some "blob" then
                    if not (stops.Contains hash) then seen.Add hash |> ignore
                else pending.Enqueue(hash, expected)
            while pending.Count > 0 do
                let hash, expected = pending.Dequeue()
                if not (stops.Contains hash) && examined.Add hash then
                    seen.Add hash |> ignore
                    match ReadObjects.readObject repo hash with
                    | Error error -> failwithf "reading %s: %s" hash error
                    | Ok value ->
                        let actual = ObjectWriter.objectTypeName value
                        match expected with
                        | Some t when t <> actual -> failwithf "object %s: expected %s, found %s" hash t actual
                        | _ -> ()
                        match value with
                        | Commit commit ->
                            enqueue commit.Tree (Some "tree")
                            for parent in commit.Parents do enqueue parent (Some "commit")
                        | Tree entries ->
                            for entry in entries do
                                if entry.Mode = 0o040000 then enqueue entry.Hash (Some "tree")
                                elif entry.Mode <> 0o160000 then enqueue entry.Hash (Some "blob")
                        | Tag tag -> enqueue tag.Object (Some tag.ObjectType)
                        | Blob _ -> ()
        walk excluded (HashSet<GitHash>()) haves
        let included = HashSet<GitHash>()
        walk included excluded wants
        included |> Seq.toArray

    /// Depth counts commits, not annotated tags or trees.
    let objectClosureShallow (repo: Repo) (wants: GitHash[]) (depth: int) : GitHash[] * GitHash[] =
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
                match ReadObjects.readObject repo hash with
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
