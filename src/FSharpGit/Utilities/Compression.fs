namespace FSharpGit

open System.IO
open System.IO.Compression

module Compression =
    
    let decompress (compressed: byte[]) : byte[] =
        use ms = new MemoryStream(compressed)
        use deflate = new DeflateStream(ms, CompressionMode.Decompress)
        use output = new MemoryStream()
        deflate.CopyTo(output)
        output.ToArray()
    
    let compress (data: byte[]) : byte[] =
        use input = new MemoryStream(data)
        use output = new MemoryStream()
        use deflate = new DeflateStream(output, CompressionLevel.Optimal)
        input.CopyTo(deflate)
        deflate.Close()
        output.ToArray()
    
    let decompressStream (stream: Stream) : byte[] =
        use deflate = new DeflateStream(stream, CompressionMode.Decompress)
        use output = new MemoryStream()
        deflate.CopyTo(output)
        output.ToArray()
    
    let compressToStream (data: byte[], stream: Stream) : unit =
        use deflate = new DeflateStream(stream, CompressionLevel.Optimal)
        deflate.Write(data, 0, data.Length)
        deflate.Close()
