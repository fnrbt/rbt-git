namespace FSharpGit

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
        use ms = new MemoryStream(compressed)
        use zlib = new ZLibStream(ms, CompressionMode.Decompress)
        use output = new MemoryStream()
        zlib.CopyTo(output)
        output.ToArray()

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
