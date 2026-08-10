namespace FSharpGit

open System
open System.IO
open System.Text

module IndexParser =
    
    let private readBytes (stream: Stream) (count: int) : byte[] =
        let buffer = Array.zeroCreate count
        let mutable offset = 0
        while offset < count do
            let read = stream.Read(buffer, offset, count - offset)
            if read = 0 then
                failwith "Unexpected end of stream"
            offset <- offset + read
        buffer
    
    let private readUInt32BE (stream: Stream) : uint32 =
        let buffer = readBytes stream 4
        BitConverter.ToUInt32(buffer |> Array.rev, 0)
    
    let private readUInt32LE (stream: Stream) : uint32 =
        let buffer = readBytes stream 4
        BitConverter.ToUInt32(buffer, 0)
    
    let private readInt64LE (stream: Stream) : int64 =
        let buffer = readBytes stream 8
        BitConverter.ToInt64(buffer, 0)
    
    let private readUInt16BE (stream: Stream) : uint16 =
        let buffer = readBytes stream 2
        BitConverter.ToUInt16(buffer |> Array.rev, 0)
    
    let private readString (stream: Stream) : string =
        let bytes = ResizeArray()
        let mutable b = readBytes stream 1 |> Array.head
        while b <> 0uy do
            bytes.Add b
            b <- readBytes stream 1 |> Array.head
        Encoding.UTF8.GetString(bytes.ToArray())
    
    let private fileTimeToDateTimeOffset (seconds: int64) (nanoseconds: int) : DateTimeOffset =
        let unixTime = seconds * 10000000L + int64 (nanoseconds / 100)
        let epoch = DateTimeOffset(1970, 1, 1, 0, 0, 0, TimeSpan.Zero)
        epoch.AddTicks(unixTime)
    
    let readIndex (repo: Repo) : Result<GitIndex, string> =
        try
            let indexPath = Repository.getIndexPath repo
            if not (File.Exists indexPath) then
                Ok { Version = 0; Entries = [||] }
            else
                use stream = File.OpenRead indexPath
                
                let signature = Encoding.UTF8.GetString(readBytes stream 4)
                if signature <> "DIRC" then
                    Error "Invalid index signature"
                else
                    let version = readUInt32BE stream |> int
                    
                    let entryCount = readUInt32BE stream |> int
                    let mutable entries = ResizeArray()
                    
                    for _ in 0 .. entryCount - 1 do
                        let ctimeSeconds = readUInt32LE stream |> int64
                        let ctimeNanoseconds = readUInt32LE stream |> int
                        let mtimeSeconds = readUInt32LE stream |> int64
                        let mtimeNanoseconds = readUInt32LE stream |> int
                        let dev = readUInt32LE stream |> int
                        let ino = readUInt32LE stream |> int
                        let mode = readUInt32LE stream |> int
                        let uid = readUInt32LE stream |> int
                        let gid = readUInt32LE stream |> int
                        let fileSize = readUInt32LE stream |> int64
                        
                        let hashBytes = readBytes stream 20
                        let hash = BitConverter.ToString(hashBytes).Replace("-", "").ToLowerInvariant()
                        
                        let flags = readUInt16BE stream
                        
                        let pathLength = int (flags &&& 0x0FFFus)
                        let stage = int ((flags >>> 12) &&& 0x0003us)
                        
                        let pathBytes = readBytes stream (int pathLength)
                        let path = Encoding.UTF8.GetString pathBytes
                        
                        let padding = 
                            if (pathLength + 1 + 62) % 8 = 0 then 0
                            else 8 - ((pathLength + 1 + 62) % 8)
                        
                        if padding > 0 then
                            readBytes stream (int padding) |> ignore
                        
                        let entry = {
                            Path = path
                            Hash = hash
                            Mode = mode
                            ModifiedTime = fileTimeToDateTimeOffset mtimeSeconds mtimeNanoseconds
                            Size = fileSize
                            Stage = stage
                        }
                        entries.Add entry
                    
                    let mutable ext = true
                    while ext && stream.Position < stream.Length - 20L do
                        let extSignature = readUInt32BE stream
                        if extSignature = 0x45434C54u then
                            let extSize = readUInt32LE stream |> int
                            readBytes stream (extSize - 4) |> ignore
                        elif extSignature = 0x52454443u then
                            let extSize = readUInt32LE stream |> int
                            readBytes stream (extSize - 4) |> ignore
                        elif extSignature = 0x55494C54u then
                            let extSize = readUInt32LE stream |> int
                            readBytes stream (extSize - 4) |> ignore
                        else
                            ext <- false
                    
                    if ext && stream.Position < stream.Length - 20L then
                        let checksum = readBytes stream 20
                        ignore checksum
                    
                    Ok { Version = version; Entries = entries.ToArray() }
        with
        | ex -> Error $"Failed to read index: {ex.Message}"
