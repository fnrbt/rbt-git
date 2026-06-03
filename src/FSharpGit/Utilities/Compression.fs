namespace FSharpGit

open System.IO
open System.IO.Compression

/// Git compresses loose objects and packed object data with zlib (RFC 1950:
/// a 2-byte header + DEFLATE body + Adler-32 trailer), NOT raw DEFLATE
/// (RFC 1951). We therefore use ZLibStream so that what we read and write is
/// byte-compatible with canonical git.
module Compression =

    let decompress (compressed: byte[]) : byte[] =
        use ms = new MemoryStream(compressed)
        use zlib = new ZLibStream(ms, CompressionMode.Decompress)
        use output = new MemoryStream()
        zlib.CopyTo(output)
        output.ToArray()

    let compress (data: byte[]) : byte[] =
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
        use zlib = new ZLibStream(stream, CompressionLevel.Optimal, leaveOpen = true)
        zlib.Write(data, 0, data.Length)
