namespace FSharpGit

open System
open System.Collections.Generic
open System.IO
open System.Security.Cryptography
open System.Text

/// Streaming packfile writer: emits the pack directly to an output Stream as it
/// is generated, with an incremental SHA-1 trailer and optional side-band-64k
/// framing. Memory stays bounded (one object plus a small per-type delta window)
/// regardless of repo size, so this scales to large repositories.
module PackStream =

    let private maxSb = 65515
    let private window = 10

    let private be32 (n: int) = [| byte (n >>> 24); byte (n >>> 16); byte (n >>> 8); byte n |]

    let private writeObjHeaderTo (ms: MemoryStream) (typeN: int) (size: int) =
        let mutable b = (typeN <<< 4) &&& 0x70
        b <- b ||| (size &&& 0x0F)
        let mutable s = size >>> 4
        if s > 0 then b <- b ||| 0x80
        ms.WriteByte(byte b)
        while s > 0 do
            let mutable c = s &&& 0x7F
            s <- s >>> 7
            if s > 0 then c <- c ||| 0x80
            ms.WriteByte(byte c)

    let private writeOfsDistance (ms: MemoryStream) (n: int) =
        let tmp = ResizeArray<byte>()
        let mutable v = n
        tmp.Add(byte (v &&& 0x7f))
        v <- (v >>> 7) - 1
        while v >= 0 do
            tmp.Add(byte ((v &&& 0x7f) ||| 0x80))
            v <- (v >>> 7) - 1
        for i in tmp.Count - 1 .. -1 .. 0 do ms.WriteByte tmp.[i]

    /// Stream a packfile for objectIds to output. Delta candidates come from a
    /// bounded per-type window, so memory does not grow with the pack size.
    let writeTo (repo: Repo) (objectIds: GitHash[]) (useDelta: bool) (useSideband: bool) (output: Stream) : unit =
        use hash = IncrementalHash.CreateHash(HashAlgorithmName.SHA1)
        let sbBuf = if useSideband then Array.zeroCreate (maxSb + 1) else [||]
        let mutable sbLen = 0
        let flushSb () =
            if useSideband && sbLen > 0 then
                sbBuf.[0] <- 1uy
                let framed = PktLine.encode sbBuf.[0..sbLen] // channel byte + sbLen data bytes
                output.Write(framed, 0, framed.Length)
                sbLen <- 0
        // Append raw bytes to channel 1 (no hashing), flushing full chunks.
        let pushChannel1 (data: byte[]) =
            let mutable i = 0
            while i < data.Length do
                let take = min (maxSb - sbLen) (data.Length - i)
                Array.blit data i sbBuf (1 + sbLen) take
                sbLen <- sbLen + take
                i <- i + take
                if sbLen = maxSb then flushSb ()
        // Emit hashed pack body bytes (framed when sideband, else raw).
        let emit (data: byte[]) =
            hash.AppendData(data, 0, data.Length)
            if useSideband then pushChannel1 data
            else output.Write(data, 0, data.Length)

        if useSideband then
            let payload = Array.append [| 2uy |] (Encoding.UTF8.GetBytes(sprintf "fsgit: packing %d objects\n" objectIds.Length))
            let framed = PktLine.encode payload
            output.Write(framed, 0, framed.Length)

        emit "PACK"B
        emit (be32 2)
        emit (be32 objectIds.Length)

        let windows = Dictionary<int, ResizeArray<int * byte[] * int>>() // type -> [(offset, content, depth)]
        let mutable packOffset = 12
        for id in objectIds do
            match ReadObjects.readRawObject repo id with
            | Ok (t, content) ->
                let tn = PackData.typeNum t
                let entryStart = packOffset
                use eb = new MemoryStream()
                let baseOpt =
                    if useDelta then
                        match windows.TryGetValue tn with
                        | true, win ->
                            let mutable res = None
                            let mutable k = win.Count - 1
                            while res.IsNone && k >= 0 do
                                let (o, c, d) = win.[k]
                                if d < 50 then res <- Some(o, c, d)
                                k <- k - 1
                            res
                        | _ -> None
                    else None
                let mutable entryDepth = 0
                match baseOpt with
                | Some (baseOff, baseContent, baseDepth) ->
                    let delta = Gc.encodeDelta baseContent content
                    let zd = Compression.compress delta
                    let zc = Compression.compress content
                    if zd.Length < zc.Length then
                        writeObjHeaderTo eb 6 delta.Length
                        writeOfsDistance eb (entryStart - baseOff)
                        eb.Write(zd, 0, zd.Length)
                        entryDepth <- baseDepth + 1
                    else
                        writeObjHeaderTo eb tn content.Length
                        eb.Write(zc, 0, zc.Length)
                | None ->
                    writeObjHeaderTo eb tn content.Length
                    let zc = Compression.compress content
                    eb.Write(zc, 0, zc.Length)
                let entryBytes = eb.ToArray()
                emit entryBytes
                packOffset <- packOffset + entryBytes.Length
                let win =
                    match windows.TryGetValue tn with
                    | true, w -> w
                    | _ -> let w = ResizeArray() in windows.[tn] <- w; w
                win.Add(entryStart, content, entryDepth)
                if win.Count > window then win.RemoveAt 0
            | Error _ -> ()

        // Trailer: SHA-1 of the pack body (NOT itself hashed); for sideband it is
        // still channel-1 data, followed by a terminating flush-pkt.
        let trailer = hash.GetHashAndReset()
        if useSideband then
            pushChannel1 trailer
            flushSb ()
            PktLine.writeFlush output
        else
            output.Write(trailer, 0, trailer.Length)
        output.Flush()
