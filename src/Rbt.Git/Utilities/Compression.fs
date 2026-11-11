namespace Rbt.Git

open System.IO
open System.IO.Compression

/// Git compresses loose objects and packed object data with zlib (RFC 1950:
/// a 2-byte header + DEFLATE body + Adler-32 trailer), NOT raw DEFLATE
/// (RFC 1951). We therefore use ZLibStream so that what we read and write is
/// byte-compatible with canonical git.
module Compression =
    let private emptyZlibMember =
        [| 0x78uy; 0x9cuy; 0x03uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x01uy |]


    let decompress (compressed: byte[]) : byte[] =
        if compressed.Length < 6 then failwith "Truncated zlib member"
        let cmf, flags = int compressed.[0], int compressed.[1]
        if cmf &&& 0x0f <> 8 || cmf >>> 4 > 7 ||
           (cmf * 256 + flags) % 31 <> 0 || flags &&& 0x20 <> 0 then
            failwith "Unsupported zlib header"
        use ms = new MemoryStream(compressed)
        use zlib = new ZLibStream(ms, CompressionMode.Decompress)
        use output = new MemoryStream()
        zlib.CopyTo(output)
        let data = output.ToArray()
        let mutable a = 1u
        let mutable b = 0u
        for value in data do
            a <- (a + uint32 value) % 65521u
            b <- (b + a) % 65521u
        let expected = (b <<< 16) ||| a
        let last = compressed.Length - 4
        let trailer =
            (uint32 compressed.[last] <<< 24) |||
            (uint32 compressed.[last + 1] <<< 16) |||
            (uint32 compressed.[last + 2] <<< 8) |||
            uint32 compressed.[last + 3]
        if expected <> trailer then failwith "Invalid zlib checksum or truncated member"
        data

    let compress (data: byte[]) : byte[] =
        // ZLibStream emits no bytes when it is disposed without a non-empty
        // write. Git packfiles still require a complete zlib member for empty
        // blobs, so provide the canonical empty member explicitly.
        if data.Length = 0 then
            Array.copy emptyZlibMember
        else
            use output = new MemoryStream()
            (
                use zlib = new ZLibStream(output, CompressionLevel.Optimal, leaveOpen = true)
                zlib.Write(data, 0, data.Length)
            )
            output.ToArray()

    let decompressStream (stream: Stream) : byte[] =
        use zlib = new ZLibStream(stream, CompressionMode.Decompress)
        use output = new MemoryStream()
        zlib.CopyTo(output)
        output.ToArray()

    let compressToStream (data: byte[], stream: Stream) : unit =
        if data.Length = 0 then
            stream.Write(emptyZlibMember)
        else
            use zlib = new ZLibStream(stream, CompressionLevel.Fastest, leaveOpen = true)
            zlib.Write(data, 0, data.Length)
