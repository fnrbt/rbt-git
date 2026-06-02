namespace FSharpGit

open System
open System.IO
open System.Text

module PackParser =
    
    let private readBytes (stream: Stream) (count: int) : byte[] =
        let buffer = Array.zeroCreate count
        let mutable offset = 0
        while offset < count do
            let read = stream.Read(buffer, offset, count - offset)
            if read = 0 then
                failwith "Unexpected end of stream"
            offset <- offset + read
        buffer
    
    let private readUInt8 (stream: Stream) : byte =
        let buffer = readBytes stream 1
        buffer.[0]
    
    let private readUInt32BE (stream: Stream) : uint32 =
        let buffer = readBytes stream 4
        BitConverter.ToUInt32(buffer |> Array.rev, 0)
    
    let private readUInt64BE (stream: Stream) : uint64 =
        let buffer = readBytes stream 8
        BitConverter.ToUInt64(buffer |> Array.rev, 0)
    
    let private readInt32BE (stream: Stream) : int32 =
        let buffer = readBytes stream 4
        BitConverter.ToInt32(buffer |> Array.rev, 0)
    
    let private readOffset (stream: Stream) : int64 * byte =
        let firstByte = readUInt8 stream
        let msb = firstByte &&& 0x80uy <> 0uy
        let mutable offset = int64 (firstByte &&& 0x7Fuy)
        let mutable shift = 7
        let mutable continuation = msb
        
        while continuation do
            let nextByte = readUInt8 stream
            continuation <- nextByte &&& 0x80uy <> 0uy
            offset <- offset ||| (int64 (nextByte &&& 0x7Fuy) <<< shift)
            shift <- shift + 7
        
        offset, firstByte
    
    let private readVarInt (stream: Stream) : int64 * int =
        let mutable value = 0L
        let mutable shift = 0
        let mutable continuation = true
        
        while continuation do
            let byte = readUInt8 stream
            continuation <- byte &&& 0x80uy <> 0uy
            value <- value ||| (int64 (byte &&& 0x7Fuy) <<< shift)
            shift <- shift + 7
        
        value, shift / 7
    
    let readPackIndex (path: string) : Result<PackIndex, string> =
        try
            use stream = File.OpenRead path
            let signature = Encoding.UTF8.GetString(readBytes stream 4)
            if signature <> "\xFFtOc" then
                Error "Invalid pack index signature"
            else
                let version = readUInt32BE stream |> int
                
                let fanout = Array.zeroCreate 256
                for i in 0 .. 255 do
                    fanout.[i] <- readUInt32BE stream |> int
                
                let totalObjects = int fanout.[255]
                let hashes = Array.zeroCreate totalObjects
                for i in 0 .. totalObjects - 1 do
                    let hashBytes = readBytes stream 20
                    hashes.[i] <- BitConverter.ToString(hashBytes).Replace("-", "").ToLowerInvariant()
                
                let crcs = Array.zeroCreate totalObjects
                for i in 0 .. totalObjects - 1 do
                    crcs.[i] <- readUInt32BE stream
                
                let offsets = Array.zeroCreate totalObjects
                for i in 0 .. totalObjects - 1 do
                    offsets.[i] <- readUInt32BE stream |> int64
                
                let largeOffsets = System.Collections.Generic.Dictionary<int32, int64>()
                if int64 stream.Position + 8L <= stream.Length then
                    let largeOffsetCount = readUInt32BE stream |> int
                    for _ in 0 .. largeOffsetCount - 1 do
                        let index = readUInt32BE stream |> int
                        let offset = readUInt64BE stream |> int64
                        largeOffsets.[index] <- offset
                
                let objects = Array.zeroCreate totalObjects
                for i in 0 .. totalObjects - 1 do
                    let offset = 
                        if offsets.[i] &&& 0x80000000L <> 0L then
                            let idx = int (offsets.[i] &&& 0x7FFFFFFFL)
                            largeOffsets.[idx]
                        else
                            offsets.[i]
                    objects.[i] <- { Hash = hashes.[i]; Offset = offset; CRC = crcs.[i] }
                
                Ok { Version = version; Fanout = fanout; Objects = objects }
        with
        | ex -> Error $"Failed to read pack index: {ex.Message}"
    

    let rec parseDelta (delta: byte[]) : byte[] * int64 * int64 =
        let mutable offset = 0
        
        let readVarInt () =
            let mutable value = 0L
            let mutable shift = 0
            let mutable continuation = true
            while continuation do
                let byte = delta.[offset]
                offset <- offset + 1
                continuation <- (byte &&& 0x80uy) <> 0uy
                value <- value ||| (int64 (byte &&& 0x7Fuy) <<< shift)
                shift <- shift + 7
            value
        
        let baseSize = readVarInt ()
        let resultSize = readVarInt ()
        
        let rec readInstructions (acc: byte[]) : byte[] =
            if offset >= delta.Length then
                acc
            else
                let cmd = delta.[offset]
                offset <- offset + 1
                if cmd &&& 0x80uy <> 0uy then
                    let mutable copyOffset = 
                        if (cmd &&& 0x01uy) <> 0uy then int delta.[offset] ||| 0
                        else 0
                    if (cmd &&& 0x01uy) <> 0uy then offset <- offset + 1
                    if (cmd &&& 0x02uy) <> 0uy then copyOffset <- copyOffset ||| (int delta.[offset] <<< 8); offset <- offset + 1
                    if (cmd &&& 0x04uy) <> 0uy then copyOffset <- copyOffset ||| (int delta.[offset] <<< 16); offset <- offset + 1
                    if (cmd &&& 0x08uy) <> 0uy then copyOffset <- copyOffset ||| (int delta.[offset] <<< 24); offset <- offset + 1
                    
                    let mutable copySize = 0
                    if (cmd &&& 0x10uy) <> 0uy then copySize <- copySize ||| int delta.[offset]; offset <- offset + 1
                    if (cmd &&& 0x20uy) <> 0uy then copySize <- copySize ||| (int delta.[offset] <<< 8); offset <- offset + 1
                    if (cmd &&& 0x40uy) <> 0uy then copySize <- copySize ||| (int delta.[offset] <<< 16); offset <- offset + 1
                    if copySize = 0 then copySize <- 0x10000
                    
                    let newData = Array.zeroCreate copySize
                    Array.Copy(acc, copyOffset, newData, 0, copySize)
                    readInstructions (Array.concat [| acc; newData |])
                elif cmd = 0uy then
                    let size = int delta.[offset]
                    offset <- offset + 1
                    let newData = delta.[offset .. offset + size - 1]
                    offset <- offset + size
                    readInstructions (Array.concat [| acc; newData |])
                else
                    let size = int cmd
                    let newData = delta.[offset .. offset + size - 1]
                    offset <- offset + size
                    readInstructions (Array.concat [| acc; newData |])
        
        readInstructions [||], baseSize, resultSize
    
    and applyBlobDelta (baseData: byte[]) (delta: byte[]) : byte[] =
        let deltaResult, baseSize, _ = parseDelta delta
        if int64 baseData.Length <> baseSize then
            failwith "Delta base size mismatch"
        deltaResult
    
    and applyTreeDelta (baseEntries: TreeEntry[]) (delta: byte[]) : TreeEntry[] =
        let resultData, baseSize, _ = parseDelta delta
        
        let baseData = 
            let ms = new System.IO.MemoryStream()
            for entry in baseEntries do
                let modeBytes = BitConverter.GetBytes entry.Mode |> Array.rev
                let pathBytes = Text.Encoding.UTF8.GetBytes entry.Path
                let hashBytes = 
                    entry.Hash 
                    |> Seq.chunkBySize 2 
                    |> Seq.map (fun pair -> System.Convert.ToByte(pair |> System.String, 16))
                    |> Seq.toArray
                
                ms.Write(modeBytes, 0, 4)
                ms.Write(pathBytes, 0, pathBytes.Length)
                ms.WriteByte(0uy)
                ms.Write(hashBytes, 0, 20)
            ms.ToArray()
        
        if int64 baseData.Length <> baseSize then
            failwith "Delta base size mismatch"
        
        let rec parseTreeEntries (offset: int) (acc: TreeEntry list) : TreeEntry list =
            if offset >= resultData.Length then
                List.rev acc
            else
                let intOffset = int offset
                let modeBytes = resultData.[intOffset .. intOffset + 3]
                let mode = BitConverter.ToInt32(modeBytes |> Array.rev, 0)
                
                let nullIndex = Array.IndexOf(resultData, 0uy, intOffset + 4)
                let path = Text.Encoding.UTF8.GetString(resultData.[intOffset + 4 .. nullIndex - 1])
                
                let hashBytes = resultData.[nullIndex + 1 .. nullIndex + 20]
                let hash = BitConverter.ToString(hashBytes).Replace("-", "").ToLowerInvariant()
                
                parseTreeEntries (nullIndex + 21) ({ Mode = mode; Path = path; Hash = hash } :: acc)
        
        parseTreeEntries 0 [] |> List.toArray
    let private readGitObject (stream: Stream) (offset: int64) (baseObj: GitObject option) : Result<GitObject, string> =
        try
            stream.Seek(offset, SeekOrigin.Begin) |> ignore
            
            let byte1 = readUInt8 stream
            let objType = (byte1 >>> 4) &&& 0x7uy
            let mutable size = int64 (byte1 &&& 0xFuy)
            let mutable shift = 4
            let mutable continuation = byte1 &&& 0x80uy <> 0uy
            
            while continuation do
                let nextByte = readUInt8 stream
                continuation <- nextByte &&& 0x80uy <> 0uy
                size <- size ||| (int64 (nextByte &&& 0x7Fuy) <<< shift)
                shift <- shift + 7
            
            let data = readBytes stream (int size)
            
            let decompressed = Compression.decompress data
            
            match objType with
            | 1uy -> Ok (Blob (ObjectParser.parseBlob decompressed))
            | 2uy -> Ok (Tree (ObjectParser.parseTree decompressed))
            | 3uy -> Ok (Commit (ObjectParser.parseCommit decompressed))
            | 4uy -> Ok (GitObject.Tag (ObjectParser.parseTag decompressed))
            | 6uy -> 
                match baseObj with
                | Some (Blob baseData) ->
                    let delta = decompressed
                    let result = applyBlobDelta baseData delta
                    Ok (Blob result)
                | _ -> Error "Invalid delta: no base object provided"
            | 7uy -> 
                match baseObj with
                | Some (Tree baseEntries) ->
                    let delta = decompressed
                    let result = applyTreeDelta baseEntries delta
                    Ok (Tree result)
                | _ -> Error "Invalid delta: no base object provided"
            | _ -> Error $"Unknown object type: {objType}"
        with        | ex -> Error $"Failed to read git object: {ex.Message}"
    
    let readPackFile (path: string) (offset: int64) : Result<GitObject, string> =
        try
            use stream = File.OpenRead path
            readGitObject stream offset None
        with
        | ex -> Error $"Failed to read pack file: {ex.Message}"
    
    let findPackObject (repo: Repo) (hash: GitHash) : Result<GitObject * string, string> =
        try
            let packFiles = Repository.packFilesExist repo
            
            let rec searchPackFile packs =
                match packs with
                | [] -> Error $"Object not found in packs: {hash}"
                | (packName, packPath) :: rest ->
                    let idxPath = Repository.getPackIndexPath repo packName
                    match readPackIndex idxPath with
                    | Ok packIndex ->
                        let objEntry: PackObjectEntry option = Array.tryFind (fun (e: PackObjectEntry) -> e.Hash = hash) packIndex.Objects
                        match objEntry with
                        | Some entry ->
                            match readPackFile packPath entry.Offset with
                            | Ok obj -> Ok (obj, packName)
                            | Error msg -> Error msg
                        | None -> searchPackFile rest
                    | Error msg -> Error msg
            
            searchPackFile (List.ofArray packFiles)
        with
        | ex -> Error $"Failed to find pack object: {ex.Message}"
