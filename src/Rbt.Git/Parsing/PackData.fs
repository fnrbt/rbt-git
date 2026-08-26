namespace Rbt.Git

open System
open System.IO
open System.IO.Compression
open System.Security.Cryptography

/// Low-level packfile primitives shared by the pack writer, the unpacker, and
/// the random-access pack reader: zlib inflate with exact byte accounting,
/// git delta application, and object-header parsing. Supports both in-memory
/// buffers and seekable streams.
module PackData =

    let typeName (n: int) =
        match n with
        | 1 -> "commit" | 2 -> "tree" | 3 -> "blob" | 4 -> "tag"
        | _ -> failwithf "unknown pack object type %d" n

    let typeNum (t: string) =
        match t with
        | "commit" -> 1 | "tree" -> 2 | "blob" -> 3 | "tag" -> 4
        | _ -> failwithf "unknown object type %s" t

    /// A stream over a slice of a buffer that yields at most one byte per Read,
    /// bounding ZLibStream look-ahead so we can recover the exact compressed length.
    type private OneByteStream(buf: byte[], start: int) =
        inherit Stream()
        let mutable p = start
        member _.Pos = p
        override _.CanRead = true
        override _.CanSeek = false
        override _.CanWrite = false
        override _.Length = int64 buf.Length
        override _.Position with get () = int64 p and set _ = ()
        override _.Flush() = ()
        override _.Seek(_, _) = raise (NotSupportedException())
        override _.SetLength _ = raise (NotSupportedException())
        override _.Write(_, _, _) = raise (NotSupportedException())
        override _.Read(buffer, offset, count) =
            if count <= 0 || p >= buf.Length then 0
            else buffer.[offset] <- buf.[p]; p <- p + 1; 1

    /// Inflate the zlib stream at `start` (uncompressed length known). Returns
    /// (data, compressedByteCount).
    let inflateAt (buf: byte[]) (start: int) (size: int) : byte[] * int =
        let obs = new OneByteStream(buf, start)
        use z = new ZLibStream(obs, CompressionMode.Decompress)
        let out = Array.zeroCreate size
        let mutable read = 0
        while read < size do
            let n = z.Read(out, read, size - read)
            if n = 0 then read <- size else read <- read + n
        let dummy = Array.zeroCreate 1
        z.Read(dummy, 0, 1) |> ignore   // consume the Adler-32 trailer
        out, (obs.Pos - start)

    /// Apply a git delta to a base buffer.
    let applyDelta (baseBuf: byte[]) (delta: byte[]) : byte[] =
        let mutable pos = 0
        let readVarint () =
            let mutable v = 0
            let mutable shift = 0
            let mutable more = true
            while more do
                let b = int delta.[pos]
                pos <- pos + 1
                v <- v ||| ((b &&& 0x7f) <<< shift)
                shift <- shift + 7
                more <- (b &&& 0x80) <> 0
            v
        let _baseSize = readVarint ()
        let resultSize = readVarint ()
        use out = new MemoryStream(resultSize)
        while pos < delta.Length do
            let op = int delta.[pos]
            pos <- pos + 1
            if op &&& 0x80 <> 0 then
                let mutable cpOff = 0
                let mutable cpSize = 0
                if op &&& 0x01 <> 0 then cpOff <- cpOff ||| (int delta.[pos]); pos <- pos + 1
                if op &&& 0x02 <> 0 then cpOff <- cpOff ||| ((int delta.[pos]) <<< 8); pos <- pos + 1
                if op &&& 0x04 <> 0 then cpOff <- cpOff ||| ((int delta.[pos]) <<< 16); pos <- pos + 1
                if op &&& 0x08 <> 0 then cpOff <- cpOff ||| ((int delta.[pos]) <<< 24); pos <- pos + 1
                if op &&& 0x10 <> 0 then cpSize <- cpSize ||| (int delta.[pos]); pos <- pos + 1
                if op &&& 0x20 <> 0 then cpSize <- cpSize ||| ((int delta.[pos]) <<< 8); pos <- pos + 1
                if op &&& 0x40 <> 0 then cpSize <- cpSize ||| ((int delta.[pos]) <<< 16); pos <- pos + 1
                let cpSize = if cpSize = 0 then 0x10000 else cpSize
                out.Write(baseBuf, cpOff, cpSize)
            elif op <> 0 then
                out.Write(delta, pos, op); pos <- pos + op
            else failwith "invalid delta opcode 0"
        out.ToArray()

    /// Read a pack object header at `pos`: (typeId, uncompressedSize, newPos).
    let readObjHeader (buf: byte[]) (pos: int) : int * int * int =
        let mutable p = pos
        let b0 = int buf.[p]
        p <- p + 1
        let typeId = (b0 >>> 4) &&& 0x07
        let mutable size = b0 &&& 0x0F
        let mutable shift = 4
        let mutable more = (b0 &&& 0x80) <> 0
        while more do
            let b = int buf.[p]
            p <- p + 1
            size <- size ||| ((b &&& 0x7f) <<< shift)
            shift <- shift + 7
            more <- (b &&& 0x80) <> 0
        typeId, size, p

    /// Read the ofs-delta base distance varint at `pos`: (relativeOffset, newPos).
    let readOfsBase (buf: byte[]) (pos: int) : int * int =
        let mutable p = pos
        let mutable c = int buf.[p]
        p <- p + 1
        let mutable rel = c &&& 0x7f
        while c &&& 0x80 <> 0 do
            c <- int buf.[p]
            p <- p + 1
            rel <- ((rel + 1) <<< 7) ||| (c &&& 0x7f)
        rel, p

    let private hashOf (buf: byte[]) (start: int) : GitHash =
        BitConverter.ToString(buf.[start .. start + 19]).Replace("-", "").ToLowerInvariant()

    // ---- streaming primitives (for unpacking a push without buffering) ----

    /// Wraps a source stream and counts bytes consumed, giving the current pack
    /// offset for ofs-delta base resolution while unpacking incrementally.
    type CountingStream(inner: Stream) =
        inherit Stream()
        let mutable count = 0
        let hasher = IncrementalHash.CreateHash(HashAlgorithmName.SHA1)
        let mutable hashing = true
        let oneByte = Array.zeroCreate<byte> 1
        member _.Count = count
        member _.StopHashing() = hashing <- false
        member _.GetHashAndReset() = hasher.GetHashAndReset()
        override _.CanRead = true
        override _.CanSeek = false
        override _.CanWrite = false
        override _.Length = 0L
        override _.Position with get () = int64 count and set _ = ()
        override _.Flush() = ()
        override _.Seek(_, _) = raise (NotSupportedException())
        override _.SetLength _ = raise (NotSupportedException())
        override _.Write(_, _, _) = raise (NotSupportedException())
        override _.Read(buffer, offset, n) =
            let r = inner.Read(buffer, offset, n)
            if r > 0 then
                count <- count + r
                if hashing then hasher.AppendData(buffer, offset, r)
            r
        override _.ReadByte() =
            let b = inner.ReadByte()
            if b >= 0 then
                count <- count + 1
                if hashing then
                    oneByte.[0] <- byte b
                    hasher.AppendData(oneByte)
            b
        override _.Dispose(disposing) =
            if disposing then hasher.Dispose()
            base.Dispose(disposing)

    /// Yields at most one byte per Read from a source stream, so ZLibStream cannot
    /// over-read past the end of one object's zlib data.
    type private OneByteSource(inner: Stream) =
        inherit Stream()
        override _.CanRead = true
        override _.CanSeek = false
        override _.CanWrite = false
        override _.Length = 0L
        override _.Position with get () = 0L and set _ = ()
        override _.Flush() = ()
        override _.Seek(_, _) = raise (NotSupportedException())
        override _.SetLength _ = raise (NotSupportedException())
        override _.Write(_, _, _) = raise (NotSupportedException())
        override _.Read(buffer, offset, count) =
            if count <= 0 then 0
            else
                let b = inner.ReadByte()
                if b < 0 then 0 else (buffer.[offset] <- byte b; 1)

    let readExact (s: Stream) (n: int) : byte[] =
        let buf = Array.zeroCreate n
        let mutable off = 0
        while off < n do
            let r = s.Read(buf, off, n - off)
            if r = 0 then failwith "unexpected end of stream"
            off <- off + r
        buf

    /// Inflate one zlib object from a stream (uncompressed size known), leaving the
    /// stream positioned just past its Adler-32 trailer.
    let inflateFromStream (inner: Stream) (size: int) : byte[] =
        let obs = new OneByteSource(inner)
        use z = new ZLibStream(obs, CompressionMode.Decompress, leaveOpen = true)
        let out = Array.zeroCreate size
        let mutable read = 0
        while read < size do
            let n = z.Read(out, read, size - read)
            if n = 0 then read <- size else read <- read + n
        let dummy = Array.zeroCreate 1
        z.Read(dummy, 0, 1) |> ignore
        out

    let readObjHeaderFromStream (s: Stream) : int * int =
        let b0 = s.ReadByte()
        if b0 < 0 then failwith "eof in object header"
        let typeId = (b0 >>> 4) &&& 0x07
        let mutable size = b0 &&& 0x0F
        let mutable shift = 4
        let mutable more = (b0 &&& 0x80) <> 0
        while more do
            let b = s.ReadByte()
            size <- size ||| ((b &&& 0x7f) <<< shift)
            shift <- shift + 7
            more <- (b &&& 0x80) <> 0
        typeId, size

    let readOfsBaseFromStream (s: Stream) : int =
        let mutable c = s.ReadByte()
        let mutable rel = c &&& 0x7f
        while c &&& 0x80 <> 0 do
            c <- s.ReadByte()
            rel <- ((rel + 1) <<< 7) ||| (c &&& 0x7f)
        rel

    /// Random-access read of the object whose entry begins at `start`, resolving
    /// ofs-deltas (via resolveByOffset) and ref-deltas (via resolveByHash).
    /// Returns (typeName, contentBytes).
    let rec readObjectAt
        (pack: byte[]) (start: int)
        (resolveByOffset: int -> (string * byte[]) option)
        (resolveByHash: GitHash -> (string * byte[]) option) : string * byte[] =
        let typeId, size, p1 = readObjHeader pack start
        match typeId with
        | 1 | 2 | 3 | 4 ->
            let content, _ = inflateAt pack p1 size
            typeName typeId, content
        | 6 ->
            let rel, p2 = readOfsBase pack p1
            let delta, _ = inflateAt pack p2 size
            match resolveByOffset (start - rel) with
            | Some (bt, bc) -> bt, applyDelta bc delta
            | None -> failwithf "ofs-delta base not found at %d" (start - rel)
        | 7 ->
            let baseHash = hashOf pack p1
            let delta, _ = inflateAt pack (p1 + 20) size
            match resolveByHash baseHash with
            | Some (bt, bc) -> bt, applyDelta bc delta
            | None -> failwithf "ref-delta base not found: %s" baseHash
        | other -> failwithf "unsupported pack object type %d" other

    /// Random-access read from a seekable pack stream without loading the
    /// complete pack into managed memory.
    let rec readObjectAtStream
        (pack: Stream) (start: int64)
        (resolveByOffset: int64 -> (string * byte[]) option)
        (resolveByHash: GitHash -> (string * byte[]) option) : string * byte[] =
        pack.Position <- start
        let typeId, size = readObjHeaderFromStream pack
        match typeId with
        | 1 | 2 | 3 | 4 ->
            typeName typeId, inflateFromStream pack size
        | 6 ->
            let rel = readOfsBaseFromStream pack
            let delta = inflateFromStream pack size
            let baseOffset = start - int64 rel
            match resolveByOffset baseOffset with
            | Some (bt, bc) -> bt, applyDelta bc delta
            | None -> failwithf "ofs-delta base not found at %d" baseOffset
        | 7 ->
            let baseHash = hashOf (readExact pack 20) 0
            let delta = inflateFromStream pack size
            match resolveByHash baseHash with
            | Some (bt, bc) -> bt, applyDelta bc delta
            | None -> failwithf "ref-delta base not found: %s" baseHash
        | other -> failwithf "unsupported pack object type %d" other

    /// Read only a packed entry's header and delta-base metadata. The returned
    /// data offset points at the existing zlib payload, which callers can copy
    /// without inflating and recompressing it.
    let internal readEntryMetadataFromStream
        (pack: Stream)
        (start: int64)
        : struct (int * int * int64 * int64 option * GitHash option) =
        pack.Position <- start
        let typeId, size = readObjHeaderFromStream pack
        match typeId with
        | 1 | 2 | 3 | 4 ->
            struct (typeId, size, pack.Position, None, None)
        | 6 ->
            let relativeOffset = readOfsBaseFromStream pack
            struct (typeId, size, pack.Position, Some(start - int64 relativeOffset), None)
        | 7 ->
            let baseHash = hashOf (readExact pack 20) 0
            struct (typeId, size, pack.Position, None, Some baseHash)
        | other ->
            failwithf "unsupported pack object type %d" other
