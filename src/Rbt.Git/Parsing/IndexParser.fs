namespace Rbt.Git

open System
open System.Buffers.Binary
open System.IO
open System.Security.Cryptography
open System.Text

module IndexParser =
    let private utf8 = UTF8Encoding(false, true)

    let private readBytes (stream: Stream) count =
        if count < 0 || int64 count > stream.Length - stream.Position then failwith "Truncated index"
        let bytes = Array.zeroCreate<byte> count
        stream.ReadExactly(bytes, 0, count)
        bytes

    let private read32 (stream: Stream) = BinaryPrimitives.ReadUInt32BigEndian(readBytes stream 4)
    let private read16 (stream: Stream) = BinaryPrimitives.ReadUInt16BigEndian(readBytes stream 2)

    let private write32 (stream: Stream) (n: uint32) =
        let bytes = Array.zeroCreate<byte> 4
        BinaryPrimitives.WriteUInt32BigEndian(bytes, n)
        stream.Write bytes

    let private write16 (stream: Stream) (n: uint16) =
        let bytes = Array.zeroCreate<byte> 2
        BinaryPrimitives.WriteUInt16BigEndian(bytes, n)
        stream.Write bytes

    let private unixTime seconds nanoseconds =
        if nanoseconds >= 1000000000u then failwith "Invalid index nanoseconds"
        DateTimeOffset.FromUnixTimeSeconds(int64 seconds).AddTicks(int64 nanoseconds / 100L)

    let private readPath (stream: Stream) (limit: int64) (length: int) =
        if length < 0xFFF then
            if length = 0 || stream.Position + int64 length >= limit then failwith "Truncated index path"
            let path = readBytes stream length
            if stream.ReadByte() <> 0 then failwith "Missing index path terminator"
            path
        else
            use output = new MemoryStream()
            let mutable ended = false
            while not ended do
                if stream.Position >= limit then failwith "Unterminated index path"
                let b = stream.ReadByte()
                if b = 0 then ended <- true else output.WriteByte(byte b)
            if output.Length < 0xFFFL then failwith "Incorrect long index path length"
            output.ToArray()

    let readIndex (repo: Repo) : Result<GitIndex, string> =
        try
            let path = Repository.getIndexPath repo
            if not (File.Exists path) then Ok { Version = 0; Entries = [||] }
            else
                let bytes = File.ReadAllBytes path
                if bytes.Length < 32 then failwith "Truncated index header or checksum"
                let contentLength = bytes.Length - 20
                let checksum = SHA1.HashData(bytes.AsSpan(0, contentLength))
                if not (bytes.AsSpan(contentLength, 20).SequenceEqual(checksum)) then failwith "Index checksum mismatch"
                use stream = new MemoryStream(bytes, 0, contentLength, false)
                if readBytes stream 4 <> Encoding.ASCII.GetBytes "DIRC" then failwith "Invalid index signature"
                let version = int (read32 stream)
                if version <> 2 && version <> 3 then failwithf "Unsupported index version %d" version
                let count = read32 stream
                if uint64 count > uint64 (contentLength - 12) / 64UL then failwith "Invalid index entry count"
                let entries = Array.zeroCreate<IndexEntry> (int count)
                for i in 0 .. entries.Length - 1 do
                    let start = stream.Position
                    let _ctimeSeconds = read32 stream
                    let _ctimeNanoseconds = read32 stream
                    let mtimeSeconds = read32 stream
                    let mtimeNanoseconds = read32 stream
                    let _dev = read32 stream
                    let _ino = read32 stream
                    let mode = int (read32 stream)
                    let _uid = read32 stream
                    let _gid = read32 stream
                    let size = int64 (read32 stream)
                    let hash = Convert.ToHexString(readBytes stream 20).ToLowerInvariant()
                    let flags = read16 stream
                    let extended = flags &&& 0x4000us <> 0us
                    if flags &&& 0x8000us <> 0us || (extended && version <> 3) then failwith "Unsupported index entry flags"
                    if extended && read16 stream <> 0us then failwith "Unsupported extended index flags"
                    let path = readPath stream (int64 contentLength) (int (flags &&& 0x0FFFus))
                    let entryLength = int (stream.Position - start)
                    let padding = (8 - entryLength % 8) % 8
                    if readBytes stream padding |> Array.exists ((<>) 0uy) then failwith "Invalid index padding"
                    entries.[i] <- {
                        Path = utf8.GetString(path)
                        Hash = hash
                        Mode = mode
                        ModifiedTime = unixTime mtimeSeconds mtimeNanoseconds
                        Size = size
                        Stage = int ((flags >>> 12) &&& 3us)
                    }
                while stream.Position < stream.Length do
                    if stream.Length - stream.Position < 8L then failwith "Truncated index extension"
                    let signature = readBytes stream 4
                    let size = read32 stream
                    if uint64 size > uint64 (stream.Length - stream.Position) then failwith "Truncated index extension body"
                    if signature.[0] < byte 'A' || signature.[0] > byte 'Z' then
                        failwithf "Unsupported required index extension: %s" (Encoding.ASCII.GetString signature)
                    stream.Position <- stream.Position + int64 size
                Ok { Version = version; Entries = entries }
        with ex -> Error $"Failed to read index: {ex.Message}"

    let writeIndex (repo: Repo) (index: GitIndex) : Result<unit, string> =
        let path = Repository.getIndexPath repo
        let lockPath = path + ".lock"
        let mutable locked = false
        try
            if index.Version <> 0 && index.Version <> 2 && index.Version <> 3 then failwith "Unsupported index version"
            Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
            use stream = new FileStream(lockPath, FileMode.CreateNew, FileAccess.ReadWrite, FileShare.None)
            locked <- true
            stream.Write(Encoding.ASCII.GetBytes "DIRC")
            write32 stream (if index.Version = 3 then 3u else 2u)
            write32 stream (uint32 index.Entries.Length)
            for entry in index.Entries do
                let start = stream.Position
                let timestamp = entry.ModifiedTime.ToUnixTimeSeconds()
                if timestamp < 0L || timestamp > int64 UInt32.MaxValue || entry.Size < 0L || entry.Size > int64 UInt32.MaxValue then
                    failwith "Index timestamp or file size out of range"
                let nanos = uint32 ((entry.ModifiedTime.Ticks % TimeSpan.TicksPerSecond) * 100L)
                write32 stream (uint32 timestamp)
                write32 stream nanos
                write32 stream (uint32 timestamp)
                write32 stream nanos
                write32 stream 0u
                write32 stream 0u
                write32 stream (uint32 entry.Mode)
                write32 stream 0u
                write32 stream 0u
                write32 stream (uint32 entry.Size)
                if not (Hashing.isValidHash entry.Hash) then failwith "Invalid index object ID"
                let objectId = Convert.FromHexString entry.Hash
                stream.Write(objectId, 0, objectId.Length)
                if entry.Stage < 0 || entry.Stage > 3 then failwith "Invalid index stage"
                let name = utf8.GetBytes entry.Path
                if Array.contains 0uy name then failwith "Invalid index path"
                write16 stream (uint16 ((entry.Stage <<< 12) ||| min name.Length 0xFFF))
                stream.Write name
                stream.WriteByte 0uy
                while (stream.Position - start) % 8L <> 0L do stream.WriteByte 0uy
            stream.Flush()
            stream.Position <- 0L
            use sha = SHA1.Create()
            let digest = sha.ComputeHash stream
            stream.Position <- stream.Length
            stream.Write digest
            stream.Flush(true)
            stream.Dispose()
            File.Move(lockPath, path, true)
            locked <- false
            Ok ()
        with ex ->
            if locked && File.Exists lockPath then File.Delete lockPath
            Error $"Failed to write index: {ex.Message}"
