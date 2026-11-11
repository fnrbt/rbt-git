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
        let mutable header = 0
        let mutable trailer = 0u
        let mutable sawEof = false
        member _.Pos = p
        member _.Header = header
        member _.Trailer = trailer
        member _.SawEof = sawEof
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
            if count <= 0 then 0
            elif p >= buf.Length then (sawEof <- true; 0)
            else
                let b = buf.[p]
                if p - start < 2 then header <- (header <<< 8) ||| int b
                trailer <- (trailer <<< 8) ||| uint32 b
                buffer.[offset] <- b
                p <- p + 1
                1

    let private verifyMember (header: int) (trailer: uint32) (count: int) (sawEof: bool) (data: byte[]) =
        let cmf, flags = header >>> 8, header &&& 0xff
        if count < 6 || sawEof || cmf &&& 0x0f <> 8 || cmf >>> 4 > 7 ||
           (cmf * 256 + flags) % 31 <> 0 || flags &&& 0x20 <> 0 then
            failwith "Truncated or invalid zlib member"
        let mutable a = 1u
        let mutable b = 0u
        for value in data do
            a <- (a + uint32 value) % 65521u
            b <- (b + a) % 65521u
        if trailer <> ((b <<< 16) ||| a) then failwith "Truncated or invalid zlib checksum"

    /// Inflate the zlib stream at `start` (uncompressed length known). Returns
    /// (data, compressedByteCount).
    /// Inflate exactly one object; reject both short and overlong data.
    let inflateAt (buf: byte[]) (start: int) (size: int) : byte[] * int =
        if start < 0 || start >= buf.Length || size < 0 then failwith "Invalid compressed object bounds"
        let obs = new OneByteStream(buf, start)
        use z = new ZLibStream(obs, CompressionMode.Decompress)
        let output = Array.zeroCreate<byte> size
        let mutable offset = 0
        while offset < size do
            let n = z.Read(output, offset, size - offset)
            if n = 0 then failwith "Truncated compressed object"
            offset <- offset + n
        let one = Array.zeroCreate<byte> 1
        if z.Read(one, 0, 1) <> 0 then failwith "Compressed object exceeds declared size"
        verifyMember obs.Header obs.Trailer (obs.Pos - start) obs.SawEof output
        output, obs.Pos - start

    let private deltaVarint (delta: byte[]) (position: byref<int>) =
        let mutable value = 0L
        let mutable shift = 0
        let mutable more = true
        while more do
            if position >= delta.Length || shift >= 63 then failwith "Invalid delta varint"
            let b = int64 delta.[position]
            position <- position + 1
            if (b &&& 0x7fL) > ((int64 Int32.MaxValue - value) >>> shift) then failwith "Delta size overflow"
            value <- value ||| ((b &&& 0x7fL) <<< shift)
            shift <- shift + 7
            more <- b &&& 0x80L <> 0L
        int value

    /// Apply a delta after validating its base, result and each instruction.
    let applyDelta (baseBuf: byte[]) (delta: byte[]) : byte[] =
        let mutable pos = 0
        let baseSize = deltaVarint delta &pos
        let resultSize = deltaVarint delta &pos
        if baseSize <> baseBuf.Length then failwith "Delta base size mismatch"
        let output = Array.zeroCreate<byte> resultSize
        let mutable written = 0
        let nextByte () =
            if pos >= delta.Length then failwith "Truncated delta instruction"
            let b = int delta.[pos]
            pos <- pos + 1
            b
        while pos < delta.Length do
            let op = nextByte ()
            if op &&& 0x80 <> 0 then
                let mutable copyOffset = 0L
                let mutable copySize = 0L
                for bit in 0 .. 3 do
                    if op &&& (1 <<< bit) <> 0 then
                        copyOffset <- copyOffset ||| (int64 (nextByte ()) <<< (bit * 8))
                for bit in 0 .. 2 do
                    if op &&& (0x10 <<< bit) <> 0 then
                        copySize <- copySize ||| (int64 (nextByte ()) <<< (bit * 8))
                if copySize = 0L then copySize <- 0x10000L
                if copyOffset > int64 baseSize || copySize > int64 baseSize - copyOffset ||
                   copySize > int64 resultSize - int64 written then failwith "Delta copy out of bounds"
                Buffer.BlockCopy(baseBuf, int copyOffset, output, written, int copySize)
                written <- written + int copySize
            elif op <> 0 then
                if op > delta.Length - pos || op > resultSize - written then failwith "Delta insert out of bounds"
                Buffer.BlockCopy(delta, pos, output, written, op)
                pos <- pos + op
                written <- written + op
            else failwith "Invalid delta opcode"
        if written <> resultSize then failwith "Delta result length mismatch"
        output

    let private readVarint (getByte: unit -> int) (first: int) (initialShift: int) =
        let mutable value = int64 first
        let mutable shift = initialShift
        let mutable more = true
        while more do
            let b = getByte ()
            if shift > 30 || (int64 (b &&& 0x7f) <<< shift) > int64 Int32.MaxValue - value then
                failwith "Pack object size overflow"
            value <- value ||| (int64 (b &&& 0x7f) <<< shift)
            shift <- shift + 7
            more <- b &&& 0x80 <> 0
        int value

    /// Read a pack entry header and reject unterminated or oversized lengths.
    let readObjHeader (buf: byte[]) (pos: int) : int * int * int =
        if pos < 0 || pos >= buf.Length then failwith "Truncated pack object header"
        let mutable p = pos
        let next () =
            if p >= buf.Length then failwith "Truncated pack object header"
            let b = int buf.[p]
            p <- p + 1
            b
        let first = next ()
        let size =
            if first &&& 0x80 = 0 then first &&& 0x0f
            else readVarint next (first &&& 0x0f) 4
        (first >>> 4) &&& 7, size, p

    let readOfsBase (buf: byte[]) (pos: int) : int * int =
        if pos < 0 || pos >= buf.Length then failwith "Truncated delta offset"
        let mutable p = pos
        let mutable c = int buf.[p]
        p <- p + 1
        let mutable rel = int64 (c &&& 0x7f)
        while c &&& 0x80 <> 0 do
            if p >= buf.Length then failwith "Truncated delta offset"
            c <- int buf.[p]
            p <- p + 1
            rel <- ((rel + 1L) <<< 7) ||| int64 (c &&& 0x7f)
            if rel > int64 Int32.MaxValue then failwith "Delta offset overflow"
        int rel, p

    let private hashOf (buf: byte[]) (start: int) : GitHash =
        if start < 0 || start > buf.Length - 20 then failwith "Truncated ref-delta object ID"
        Convert.ToHexString(buf, start, 20).ToLowerInvariant()

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
                if r > Int32.MaxValue - count then failwith "Pack stream offset overflow"
                count <- count + r
                if hashing then hasher.AppendData(buffer, offset, r)
            r
        override _.ReadByte() =
            let b = inner.ReadByte()
            if b >= 0 then
                if count = Int32.MaxValue then failwith "Pack stream offset overflow"
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
        let mutable consumed = 0
        let mutable header = 0
        let mutable trailer = 0u
        let mutable sawEof = false
        member _.Consumed = consumed
        member _.Header = header
        member _.Trailer = trailer
        member _.SawEof = sawEof
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
                if b < 0 then (sawEof <- true; 0)
                else
                    if consumed < 2 then header <- (header <<< 8) ||| b
                    trailer <- (trailer <<< 8) ||| uint32 b
                    buffer.[offset] <- byte b
                    consumed <- consumed + 1
                    1

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
        if size < 0 then failwith "Invalid compressed object size"
        let obs = new OneByteSource(inner)
        use z = new ZLibStream(obs, CompressionMode.Decompress, leaveOpen = true)
        let output = Array.zeroCreate<byte> size
        let mutable offset = 0
        while offset < size do
            let n = z.Read(output, offset, size - offset)
            if n = 0 then failwith "Truncated compressed object"
            offset <- offset + n
        let one = Array.zeroCreate<byte> 1
        if z.Read(one, 0, 1) <> 0 then failwith "Compressed object exceeds declared size"
        verifyMember obs.Header obs.Trailer obs.Consumed obs.SawEof output
        output

    let readObjHeaderFromStream (s: Stream) : int * int =
        let next () =
            let b = s.ReadByte()
            if b < 0 then failwith "Truncated pack object header"
            b
        let first = next ()
        let size =
            if first &&& 0x80 = 0 then first &&& 0x0f
            else readVarint next (first &&& 0x0f) 4
        (first >>> 4) &&& 7, size

    let readOfsBaseFromStream (s: Stream) : int =
        let mutable c = s.ReadByte()
        if c < 0 then failwith "Truncated delta offset"
        let mutable rel = int64 (c &&& 0x7f)
        while c &&& 0x80 <> 0 do
            c <- s.ReadByte()
            if c < 0 then failwith "Truncated delta offset"
            rel <- ((rel + 1L) <<< 7) ||| int64 (c &&& 0x7f)
            if rel > int64 Int32.MaxValue then failwith "Delta offset overflow"
        int rel

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
            if rel <= 0 || rel > start then failwith "Invalid ofs-delta base offset"
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
            if rel <= 0 || int64 rel > start then failwith "Invalid ofs-delta base offset"
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
