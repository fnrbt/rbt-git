namespace Rbt.Git

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO

/// Correct random-access reader for objects stored in packfiles. Resolves
/// ofs-deltas (within the pack) and ref-deltas (within the pack, or against
/// loose objects / other packs for thin packs). Replaces the broken
/// PackParser.findPackObject read path. Pack files are immutable once written
/// (gc creates a uniquely named pack), so parsed indexes are cached by path.
/// Pack contents remain on disk and are read through a bounded FileStream.
module PackStore =

    // packPath -> hash -> offset
    let private cache = ConcurrentDictionary<string, Dictionary<GitHash, int64>>()

    let private loadIndex (packPath: string) (idxPath: string) : Dictionary<GitHash, int64> =
        cache.GetOrAdd(packPath, fun _ ->
            let map = Dictionary<GitHash, int64>()
            match PackParser.readPackIndex idxPath with
            | Ok idx -> for o in idx.Objects do map.[o.Hash] <- o.Offset
            | Error _ -> ()
            map)


    let private tryLoose (repo: Repo) (hash: GitHash) : (string * byte[]) option =
        let path = Repository.getLooseObjectPath repo hash
        if File.Exists path then
            let dec = Compression.decompress (File.ReadAllBytes path)
            let nul = System.Array.IndexOf(dec, 0uy)
            if nul < 0 then None
            else
                let header = System.Text.Encoding.UTF8.GetString(dec, 0, nul)
                let sp = header.IndexOf ' '
                let t = if sp < 0 then header else header.Substring(0, sp)
                Some(t, dec.[nul + 1 ..])
        else None
    type private SessionPack = {
        Ordinal: int
        Path: string
        Stream: FileStream
        Index: Dictionary<GitHash, int64>
        EndOffsets: Dictionary<int64, int64>
        HashesByOffset: Dictionary<int64, GitHash>
    }

    /// Reuses parsed indexes and open pack streams while reading a batch of
    /// objects. A bounded reconstructed-object cache prevents every delta child
    /// from reopening and reinflating the same base chain.
    type ReadSession(repo: Repo) =
        let maxCacheBytes = 128 * 1024 * 1024
        let packs =
            Repository.packFilesExist repo
            |> Array.mapi (fun ordinal (name, packPath) ->
                let idxPath = Repository.getPackIndexPath repo name
                let index = loadIndex packPath idxPath
                if index.Count = 0 then None
                else
                    let ordered =
                        index
                        |> Seq.map (fun pair -> pair.Key, pair.Value)
                        |> Seq.sortBy snd
                        |> Seq.toArray
                    let endOffsets = Dictionary<int64, int64>()
                    let hashesByOffset = Dictionary<int64, GitHash>()
                    let packDataEnd = FileInfo(packPath).Length - 20L
                    for position in 0 .. ordered.Length - 1 do
                        let hash, offset = ordered.[position]
                        let nextOffset =
                            if position + 1 < ordered.Length then snd ordered.[position + 1]
                            else packDataEnd
                        endOffsets.[offset] <- nextOffset
                        hashesByOffset.[offset] <- hash
                    Some {
                        Ordinal = ordinal
                        Path = packPath
                        Stream =
                            new FileStream(
                                packPath,
                                FileMode.Open,
                                FileAccess.Read,
                                FileShare.Read,
                                256 * 1024,
                                FileOptions.RandomAccess)
                        Index = index
                        EndOffsets = endOffsets
                        HashesByOffset = hashesByOffset
                    })
            |> Array.choose id
        let locations = Dictionary<GitHash, struct (SessionPack * int64)>()
        let reconstructed = Dictionary<struct (string * int64), string * byte[]>()
        let cacheOrder = Queue<struct (string * int64)>()
        let mutable cacheBytes = 0

        do
            for pack in packs do
                for pair in pack.Index do
                    locations.TryAdd(pair.Key, struct (pack, pair.Value)) |> ignore

        let cacheValue (key: struct (string * int64)) (value: string * byte[]) =
            let _, content = value
            if content.Length <= maxCacheBytes then
                while cacheOrder.Count > 0 && cacheBytes + content.Length > maxCacheBytes do
                    let oldest = cacheOrder.Dequeue()
                    match reconstructed.TryGetValue oldest with
                    | true, (_, evicted) ->
                        reconstructed.Remove oldest |> ignore
                        cacheBytes <- cacheBytes - evicted.Length
                    | _ -> ()
                reconstructed.[key] <- value
                cacheOrder.Enqueue key
                cacheBytes <- cacheBytes + content.Length

        let rec tryReadAt (pack: SessionPack) offset =
            let key = struct (pack.Path, offset)
            match reconstructed.TryGetValue key with
            | true, value -> Some value
            | _ ->
                let value =
                    PackData.readObjectAtStream
                        pack.Stream
                        offset
                        (tryReadAt pack)
                        tryReadHash
                cacheValue key value
                Some value

        and tryReadHash hash =
            match locations.TryGetValue hash with
            | true, struct (pack, offset) -> tryReadAt pack offset
            | _ -> tryLoose repo hash

        let copyBuffer = Array.zeroCreate<byte> (256 * 1024)

        let objectHeader typeNumber size =
            let bytes = ResizeArray<byte>()
            let mutable first = (typeNumber <<< 4) ||| (size &&& 0x0f)
            let mutable remaining = size >>> 4
            if remaining > 0 then first <- first ||| 0x80
            bytes.Add(byte first)
            while remaining > 0 do
                let mutable current = remaining &&& 0x7f
                remaining <- remaining >>> 7
                if remaining > 0 then current <- current ||| 0x80
                bytes.Add(byte current)
            bytes.ToArray()

        let copyRange
            (pack: SessionPack)
            (startOffset: int64)
            (endOffset: int64)
            (write: byte[] -> int -> int -> unit) =
            pack.Stream.Position <- startOffset
            let mutable remaining = endOffset - startOffset
            while remaining > 0L do
                let wanted = min copyBuffer.Length (int (min remaining (int64 Int32.MaxValue)))
                let read = pack.Stream.Read(copyBuffer, 0, wanted)
                if read = 0 then raise (EndOfStreamException("Unexpected end of packed object"))
                write copyBuffer 0 read
                remaining <- remaining - int64 read

        member _.OrderForStreaming(objectIds: GitHash[]) =
            objectIds
            |> Array.sortBy (fun hash ->
                match locations.TryGetValue hash with
                | true, struct (pack, offset) -> struct (pack.Ordinal, offset)
                | _ -> struct (Int32.MaxValue, Int64.MaxValue))

        member _.TryCopyPackedEntry
            (hash: GitHash)
            (included: HashSet<GitHash>)
            (write: byte[] -> int -> int -> unit) =
            try
                match locations.TryGetValue hash with
                | false, _ -> false
                | true, struct (pack, offset) ->
                    let endOffset = pack.EndOffsets.[offset]
                    let struct (typeId, size, dataOffset, baseOffset, baseHash) =
                        PackData.readEntryMetadataFromStream pack.Stream offset
                    match typeId with
                    | 1 | 2 | 3 | 4 ->
                        copyRange pack offset endOffset write
                        true
                    | 6 ->
                        match baseOffset with
                        | Some value ->
                            match pack.HashesByOffset.TryGetValue value with
                            | true, baseObjectHash when included.Contains baseObjectHash ->
                                let header = objectHeader 7 size
                                write header 0 header.Length
                                let hashBytes = Convert.FromHexString baseObjectHash
                                write hashBytes 0 hashBytes.Length
                                copyRange pack dataOffset endOffset write
                                true
                            | _ -> false
                        | None -> false
                    | 7 ->
                        match baseHash with
                        | Some value when included.Contains value ->
                            copyRange pack offset endOffset write
                            true
                        | _ -> false
                    | _ -> false
            with _ -> false

        member _.TryRead(hash: GitHash) =
            try tryReadHash hash
            with _ -> None

        interface IDisposable with
            member _.Dispose() =
                for pack in packs do pack.Stream.Dispose()

    /// Read an object by hash from any packfile in the repo. None if not packed.
    let rec tryRead (repo: Repo) (hash: GitHash) : (string * byte[]) option =
        let packs = Repository.packFilesExist repo
        let mutable result = None
        let mutable i = 0
        while result.IsNone && i < packs.Length do
            let (name, packPath) = packs.[i]
            let idxPath = Repository.getPackIndexPath repo name
            let map = loadIndex packPath idxPath
            match map.TryGetValue hash with
            | true, off ->
                use pack =
                    new FileStream(
                        packPath,
                        FileMode.Open,
                        FileAccess.Read,
                        FileShare.Read,
                        64 * 1024,
                        FileOptions.RandomAccess)
                // Resolve bases: ofs-delta by offset within THIS pack; ref-delta by
                // hash within this pack, else loose / other packs (thin packs).
                let rec byOff (o: int64) : (string * byte[]) option =
                    Some(PackData.readObjectAtStream pack o byOff byHash)
                and byHash (h: GitHash) : (string * byte[]) option =
                    match map.TryGetValue h with
                    | true, o2 -> byOff o2
                    | _ ->
                        match tryLoose repo h with
                        | Some r -> Some r
                        | None -> tryRead repo h
                result <- Some(PackData.readObjectAtStream pack off byOff byHash)
            | _ -> ()
            i <- i + 1
        result

    let exists (repo: Repo) (hash: GitHash) : bool =
        (tryRead repo hash).IsSome
