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

    let private collectTree (repo: Repo) (treeHash: GitHash) (acc: HashSet<GitHash>) =
        let rec go (h: GitHash) =
            if acc.Add h then
                match ReadObjects.readTree repo h with
                | Ok entries ->
                    for e in entries do
                        if e.Mode = 0o40000 then go e.Hash          // subtree
                        elif e.Mode = 0o160000 then ()              // submodule commit: not our object
                        else acc.Add e.Hash |> ignore               // blob / symlink
                | Error _ -> ()
        go treeHash

    /// All object ids needed to send the given commit tips, excluding history
    /// reachable from `haves`.
    let objectClosure (repo: Repo) (wants: GitHash[]) (haves: GitHash[]) : GitHash[] =
        let havesSet = HashSet<GitHash>(haves)
        // A common commit means the client also has every tree and blob in
        // that commit's snapshot. Excluding only the commit IDs still resends
        // the entire unchanged working tree on every incremental fetch.
        let haveObjects = HashSet<GitHash>()
        for have in haves do
            match ReadObjects.readCommit repo have with
            | Ok commit -> collectTree repo commit.Tree haveObjects
            | Error _ -> ()
        let commitsSeen = HashSet<GitHash>()
        let commits = ResizeArray<GitHash>()
        let queue = Queue<GitHash>(wants)
        while queue.Count > 0 do
            let c = queue.Dequeue()
            if not (havesSet.Contains c) && commitsSeen.Add c then
                match ReadObjects.readCommit repo c with
                | Ok commit ->
                    commits.Add c
                    for p in commit.Parents do queue.Enqueue p
                | Error _ ->
                    // A tip that is not a commit (e.g. a lightweight tag points straight
                    // at a tree/blob, or an annotated tag object). Keep it as a raw object.
                    commits.Add c
        let objs = HashSet<GitHash>()
        for c in commits do
            objs.Add c |> ignore
            match ReadObjects.readCommit repo c with
            | Ok commit -> collectTree repo commit.Tree objs
            | Error _ ->
                // annotated tag object: include it and peel to its target
                match ReadObjects.readTag repo c with
                | Ok tag ->
                    objs.Add tag.Object |> ignore
                    match ReadObjects.readObject repo tag.Object with
                    | Ok (Commit tc) -> collectTree repo tc.Tree objs
                    | _ -> ()
                | Error _ -> ()
        objs.ExceptWith haveObjects
        objs |> Seq.toArray

    /// Depth-limited closure for shallow clone. Returns (objectIds, shallowBoundary):
    /// the commits at exactly `depth` whose parents are omitted.
    let objectClosureShallow (repo: Repo) (wants: GitHash[]) (depth: int) : GitHash[] * GitHash[] =
        let included = HashSet<GitHash>()
        let shallow = HashSet<GitHash>()
        let q = Queue<GitHash * int>()
        for w in wants do q.Enqueue(w, 1)
        while q.Count > 0 do
            let (c, d) = q.Dequeue()
            if included.Add c then
                match ReadObjects.readCommit repo c with
                | Ok commit ->
                    if d >= depth then
                        if commit.Parents.Length > 0 then shallow.Add c |> ignore
                    else
                        for p in commit.Parents do q.Enqueue(p, d + 1)
                | Error _ -> ()
        let objs = HashSet<GitHash>()
        for c in included do
            objs.Add c |> ignore
            match ReadObjects.readCommit repo c with
            | Ok commit -> collectTree repo commit.Tree objs
            | Error _ -> ()
        objs |> Seq.toArray, shallow |> Seq.toArray

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
        writePackFor repo (objectClosure repo wants haves)

    /// Like writePack, but delta-compresses the pack (ofs-delta) when useDelta is set.
    let writePackOpt (repo: Repo) (wants: GitHash[]) (haves: GitHash[]) (useDelta: bool) : Result<byte[], string> =
        try Ok(fst (Gc.buildPack repo (objectClosure repo wants haves) useDelta))
        with ex -> Error $"writePack failed: {ex.Message}"

    // ---- pack unpacking (receive) -----------------------------------------

    /// Unpack a received packfile into loose objects. Resolves ref-deltas (by
    /// hash, possibly against objects already in the repo — i.e. thin packs) and
    /// ofs-deltas (by offset within the pack). Returns the written object ids.
    let unpackPack (repo: Repo) (pack: byte[]) : Result<GitHash[], string> =
        try
            if pack.Length < 12 || pack.[0] <> byte 'P' || pack.[1] <> byte 'A' || pack.[2] <> byte 'C' || pack.[3] <> byte 'K' then
                Error "not a packfile"
            else
                let count =
                    (int pack.[8] <<< 24) ||| (int pack.[9] <<< 16) ||| (int pack.[10] <<< 8) ||| (int pack.[11])
                // offset -> (type, content) for ofs-delta base resolution
                let byOffset = Dictionary<int, string * byte[]>()
                let written = ResizeArray<GitHash>()
                let mutable pos = 12
                let resolveRefBase (h: GitHash) : (string * byte[]) option =
                    match ReadObjects.readRawObject repo h with
                    | Ok (t, c) -> Some (t, c)
                    | Error _ -> None
                let mutable err = None
                let mutable i = 0
                while i < count && err.IsNone do
                    let entryStart = pos
                    let typeId, size, p1 = PackData.readObjHeader pack pos
                    pos <- p1
                    match typeId with
                    | 1 | 2 | 3 | 4 ->
                        let content, consumed = PackData.inflateAt pack pos size
                        pos <- pos + consumed
                        let t = PackData.typeName typeId
                        byOffset.[entryStart] <- (t, content)
                        match ObjectWriter.writeRaw repo t content with
                        | Ok h -> written.Add h
                        | Error e -> err <- Some e
                    | 6 -> // ofs-delta: base is at (entryStart - rel)
                        let rel, p2 = PackData.readOfsBase pack pos
                        pos <- p2
                        let baseOffset = entryStart - rel
                        let delta, consumed = PackData.inflateAt pack pos size
                        pos <- pos + consumed
                        match byOffset.TryGetValue baseOffset with
                        | true, (bt, bc) ->
                            let result = PackData.applyDelta bc delta
                            byOffset.[entryStart] <- (bt, result)
                            match ObjectWriter.writeRaw repo bt result with
                            | Ok h -> written.Add h
                            | Error e -> err <- Some e
                        | _ -> err <- Some (sprintf "ofs-delta base not found at offset %d" baseOffset)
                    | 7 -> // ref-delta: 20-byte base hash precedes the delta
                        let baseHash =
                            BitConverter.ToString(pack.[pos .. pos + 19]).Replace("-", "").ToLowerInvariant()
                        pos <- pos + 20
                        let delta, consumed = PackData.inflateAt pack pos size
                        pos <- pos + consumed
                        // Earlier pack entries are written to disk as we go, so the
                        // base (whether already in the repo or earlier in this pack)
                        // is found by hash via the repo reader.
                        let baseOpt = resolveRefBase baseHash
                        match baseOpt with
                        | Some (bt, bc) ->
                            let result = PackData.applyDelta bc delta
                            byOffset.[entryStart] <- (bt, result)
                            match ObjectWriter.writeRaw repo bt result with
                            | Ok h -> written.Add h
                            | Error e -> err <- Some e
                        | None -> err <- Some (sprintf "ref-delta base not found: %s" baseHash)
                    | other -> err <- Some (sprintf "unsupported pack object type %d" other)
                    i <- i + 1
                match err with
                | Some e -> Error e
                | None -> Ok (written.ToArray())
        with ex -> Error $"unpackPack failed: {ex.Message}"

    /// Stream-unpack a received packfile into `targetRepo`, resolving thin-pack
    /// bases from `sourceRepo`. The complete pack trailer is verified before
    /// success is returned.
    let unpackPackStreamInto
        (sourceRepo: Repo)
        (targetRepo: Repo)
        (counting: PackData.CountingStream) : Result<GitHash[], string> =
        try
            let magic =
                try PackData.readExact counting 4 with _ -> [||]
            if magic.Length < 4 then
                Ok [||]
            elif not (magic.[0] = byte 'P' && magic.[1] = byte 'A' && magic.[2] = byte 'C' && magic.[3] = byte 'K') then
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
                    let byOffset = Dictionary<int, GitHash>()
                    let written = ResizeArray<GitHash>()
                    let mutable err = None
                    let mutable i = 0
                    let readBase hash =
                        match ReadObjects.readRawObject targetRepo hash with
                        | Ok value -> Ok value
                        | Error _ -> ReadObjects.readRawObject sourceRepo hash
                    let writeObj (t: string) (content: byte[]) (entryStart: int) =
                        match ObjectWriter.writeRaw targetRepo t content with
                        | Ok hash ->
                            byOffset.[entryStart] <- hash
                            written.Add hash
                        | Error error -> err <- Some error
                    while i < count && err.IsNone do
                        let entryStart = counting.Count
                        let typeId, size = PackData.readObjHeaderFromStream counting
                        match typeId with
                        | 1 | 2 | 3 | 4 ->
                            let content = PackData.inflateFromStream counting size
                            writeObj (PackData.typeName typeId) content entryStart
                        | 6 ->
                            let rel = PackData.readOfsBaseFromStream counting
                            let delta = PackData.inflateFromStream counting size
                            let baseOffset = entryStart - rel
                            match byOffset.TryGetValue baseOffset with
                            | true, baseHash ->
                                match readBase baseHash with
                                | Ok (baseType, baseContent) ->
                                    writeObj baseType (PackData.applyDelta baseContent delta) entryStart
                                | Error error -> err <- Some error
                            | _ -> err <- Some(sprintf "ofs-delta base not found at offset %d" baseOffset)
                        | 7 ->
                            let baseHash =
                                BitConverter.ToString(PackData.readExact counting 20).Replace("-", "").ToLowerInvariant()
                            let delta = PackData.inflateFromStream counting size
                            match readBase baseHash with
                            | Ok (baseType, baseContent) ->
                                writeObj baseType (PackData.applyDelta baseContent delta) entryStart
                            | Error _ -> err <- Some(sprintf "ref-delta base not found: %s" baseHash)
                        | other -> err <- Some(sprintf "unsupported pack object type %d" other)
                        i <- i + 1

                    counting.StopHashing()
                    let trailer = PackData.readExact counting 20
                    let computed = counting.GetHashAndReset()
                    if not (CryptographicOperations.FixedTimeEquals(trailer, computed)) then
                        Error "pack trailer checksum mismatch"
                    elif counting.ReadByte() >= 0 then
                        Error "trailing data after pack checksum"
                    else
                        match err with
                        | Some error -> Error error
                        | None -> Ok(written.ToArray())
        with ex -> Error $"unpackPackStream failed: {ex.Message}"

    let unpackPackStream (repo: Repo) (counting: PackData.CountingStream) : Result<GitHash[], string> =
        unpackPackStreamInto repo repo counting

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
