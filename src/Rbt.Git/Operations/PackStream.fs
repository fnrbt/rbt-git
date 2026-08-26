namespace Rbt.Git

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

    let private writeOfsDistance (ms: MemoryStream) (n: int64) =
        let tmp = ResizeArray<byte>()
        let mutable v = n
        tmp.Add(byte (v &&& 0x7fL))
        v <- (v >>> 7) - 1L
        while v >= 0L do
            tmp.Add(byte ((v &&& 0x7fL) ||| 0x80L))
            v <- (v >>> 7) - 1L
        for i in tmp.Count - 1 .. -1 .. 0 do ms.WriteByte tmp.[i]

    type private WriteOnlyStream(write: byte[] -> int -> int -> unit) =
        inherit Stream()
        override _.CanRead = false
        override _.CanSeek = false
        override _.CanWrite = true
        override _.Length = raise (NotSupportedException())
        override _.Position
            with get () = raise (NotSupportedException())
            and set _ = raise (NotSupportedException())
        override _.Flush() = ()
        override _.Read(_, _, _) = raise (NotSupportedException())
        override _.Seek(_, _) = raise (NotSupportedException())
        override _.SetLength _ = raise (NotSupportedException())
        override _.Write(buffer, offset, count) = write buffer offset count

    /// Stream a packfile for objectIds to output. Delta candidates come from a
    /// count- and byte-bounded per-type window. Objects larger than the window
    /// budget are compressed directly to the output and never retained.
    let writeTo (repo: Repo) (objectIds: GitHash[]) (useDelta: bool) (useSideband: bool) (output: Stream) : unit =
        use hash = IncrementalHash.CreateHash(HashAlgorithmName.SHA1)
        let sbBuf = if useSideband then Array.zeroCreate (maxSb + 1) else [||]
        let mutable sbLen = 0
        let flushSb () =
            if useSideband && sbLen > 0 then
                sbBuf.[0] <- 1uy
                let framed = PktLine.encode sbBuf.[0..sbLen]
                output.Write(framed, 0, framed.Length)
                sbLen <- 0
        let pushChannel1 (data: byte[]) (offset: int) (count: int) =
            let mutable sourceOffset = offset
            let endOffset = offset + count
            while sourceOffset < endOffset do
                let take = min (maxSb - sbLen) (endOffset - sourceOffset)
                Array.blit data sourceOffset sbBuf (1 + sbLen) take
                sbLen <- sbLen + take
                sourceOffset <- sourceOffset + take
                if sbLen = maxSb then flushSb ()
        let emitSlice (data: byte[]) (offset: int) (count: int) =
            hash.AppendData(data, offset, count)
            if useSideband then pushChannel1 data offset count
            else output.Write(data, offset, count)
        let emit (data: byte[]) = emitSlice data 0 data.Length

        if useSideband then
            let payload = Array.append [| 2uy |] (Encoding.UTF8.GetBytes(sprintf "rbt-git: packing %d objects\n" objectIds.Length))
            let framed = PktLine.encode payload
            output.Write(framed, 0, framed.Length)

        emit "PACK"B
        emit (be32 2)
        emit (be32 objectIds.Length)

        let maxWindowBytesPerType = 16 * 1024 * 1024
        let windows = Dictionary<int, ResizeArray<int64 * byte[] * int>>()
        let windowBytes = Dictionary<int, int>()
        let mutable packOffset = 12L
        use entryOutput =
            new WriteOnlyStream(fun data offset count ->
                emitSlice data offset count
                packOffset <- packOffset + int64 count)

        use reader = new PackStore.ReadSession(repo)

        let writeWholeObject (typeNumber: int) (content: byte[]) =
            use header = new MemoryStream()
            writeObjHeaderTo header typeNumber content.Length
            let headerBytes = header.ToArray()
            emit headerBytes
            packOffset <- packOffset + int64 headerBytes.Length
            Compression.compressToStream(content, entryOutput)

        let orderedIds = reader.OrderForStreaming objectIds
        let included = HashSet<GitHash>(objectIds)
        let copyEntry data offset count =
            emitSlice data offset count
            packOffset <- packOffset + int64 count

        for id in orderedIds do
            if not (reader.TryCopyPackedEntry id included copyEntry) then
                match reader.TryRead id with
                | Some (t, content) ->
                    let tn = PackData.typeNum t
                    let entryStart = packOffset
                    let baseOpt =
                        if useDelta && content.Length <= maxWindowBytesPerType then
                            match windows.TryGetValue tn with
                            | true, win when win.Count > 0 -> Some win.[win.Count - 1]
                            | _ -> None
                        else None
                    let mutable entryDepth = 0
                    match baseOpt with
                    | Some (baseOff, baseContent, baseDepth) when baseDepth < 50 ->
                        let delta = Gc.encodeDelta baseContent content
                        let zd = Compression.compress delta
                        let zc = Compression.compress content
                        use entry = new MemoryStream()
                        if zd.Length < zc.Length then
                            writeObjHeaderTo entry 6 delta.Length
                            writeOfsDistance entry (entryStart - baseOff)
                            entry.Write(zd, 0, zd.Length)
                            entryDepth <- baseDepth + 1
                        else
                            writeObjHeaderTo entry tn content.Length
                            entry.Write(zc, 0, zc.Length)
                        let entryBytes = entry.ToArray()
                        emit entryBytes
                        packOffset <- packOffset + int64 entryBytes.Length
                    | _ ->
                        writeWholeObject tn content

                    if content.Length <= maxWindowBytesPerType then
                        let win =
                            match windows.TryGetValue tn with
                            | true, existing -> existing
                            | _ ->
                                let created = ResizeArray()
                                windows.[tn] <- created
                                created
                        let mutable retainedBytes =
                            match windowBytes.TryGetValue tn with
                            | true, value -> value
                            | _ -> 0
                        while win.Count > 0
                              && (win.Count >= window
                                  || retainedBytes + content.Length > maxWindowBytesPerType) do
                            let _, evicted, _ = win.[0]
                            win.RemoveAt 0
                            retainedBytes <- retainedBytes - evicted.Length
                        win.Add(entryStart, content, entryDepth)
                        windowBytes.[tn] <- retainedBytes + content.Length
                | None -> ()

        let trailer = hash.GetHashAndReset()
        if useSideband then
            pushChannel1 trailer 0 trailer.Length
            flushSb ()
            PktLine.writeFlush output
        else
            output.Write(trailer, 0, trailer.Length)
        output.Flush()
