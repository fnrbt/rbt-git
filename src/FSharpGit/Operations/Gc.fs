namespace FSharpGit

open System
open System.Collections.Generic
open System.IO
open System.Security.Cryptography

/// Packfile generation with delta compression, packfile index (.idx v2) writing,
/// and gc/repack (collapse loose objects + existing packs into one delta pack and
/// drop the originals). The delta encoder is verified by round-trip through
/// PackData.applyDelta and, end to end, by `git fsck`/`git clone` of repacked repos.
module Gc =

    // ---- low-level encoders ----------------------------------------------

    let private crcTable =
        lazy
            (Array.init 256 (fun n ->
                let mutable c = uint32 n
                for _ in 0 .. 7 do
                    c <- if c &&& 1u <> 0u then 0xEDB88320u ^^^ (c >>> 1) else c >>> 1
                c))

    let crc32 (data: byte[]) : uint32 =
        let t = crcTable.Value
        let mutable c = 0xFFFFFFFFu
        for b in data do
            c <- t.[int ((c ^^^ uint32 b) &&& 0xFFu)] ^^^ (c >>> 8)
        c ^^^ 0xFFFFFFFFu

    let private be32 (n: int) : byte[] = [| byte (n >>> 24); byte (n >>> 16); byte (n >>> 8); byte n |]
    let private beU32 (n: uint32) : byte[] = [| byte (n >>> 24); byte (n >>> 16); byte (n >>> 8); byte n |]
    let private hexToBytes (h: string) : byte[] = Array.init (h.Length / 2) (fun i -> Convert.ToByte(h.Substring(i * 2, 2), 16))

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

    /// Encode the ofs-delta base distance (inverse of PackData.readOfsBase).
    let private writeOfsDistance (ms: MemoryStream) (n: int) =
        let tmp = ResizeArray<byte>()
        let mutable v = n
        tmp.Add(byte (v &&& 0x7f))
        v <- (v >>> 7) - 1
        while v >= 0 do
            tmp.Add(byte ((v &&& 0x7f) ||| 0x80))
            v <- (v >>> 7) - 1
        for i in tmp.Count - 1 .. -1 .. 0 do ms.WriteByte tmp.[i]

    /// Produce a git delta from base -> target (copy/insert ops). Correct for any
    /// inputs; compression quality depends on 16-byte block matches.
    let encodeDelta (b: byte[]) (t: byte[]) : byte[] =
        let out = ResizeArray<byte>()
        let writeVarint (n: int) =
            let mutable v = n
            let mutable more = true
            while more do
                let c = v &&& 0x7f
                v <- v >>> 7
                if v > 0 then out.Add(byte (c ||| 0x80))
                else (out.Add(byte c); more <- false)
        writeVarint b.Length
        writeVarint t.Length
        let W = 16
        let index = Dictionary<int, int>()
        let fp (arr: byte[]) (off: int) =
            let mutable h = 0
            for k in 0 .. W - 1 do h <- h * 31 + int arr.[off + k]
            h
        if b.Length >= W then
            for i in 0 .. b.Length - W do index.[fp b i] <- i
        let literals = ResizeArray<byte>()
        let flush () =
            let mutable idx = 0
            while idx < literals.Count do
                let n = min 127 (literals.Count - idx)
                out.Add(byte n)
                for k in 0 .. n - 1 do out.Add(literals.[idx + k])
                idx <- idx + n
            literals.Clear()
        let mutable i = 0
        while i < t.Length do
            let mutable matchOff = -1
            let mutable matchLen = 0
            if i + W <= t.Length then
                match index.TryGetValue(fp t i) with
                | true, bo when bo + W <= b.Length && Array.sub b bo W = Array.sub t i W ->
                    let mutable len = W
                    while bo + len < b.Length && i + len < t.Length && b.[bo + len] = t.[i + len] do len <- len + 1
                    matchOff <- bo; matchLen <- len
                | _ -> ()
            if matchLen >= W then
                flush ()
                let mutable srcOff = matchOff
                let mutable remaining = matchLen
                while remaining > 0 do
                    let chunk = min 0xFFFFFF remaining
                    let mutable op = 0x80
                    let ob = ResizeArray<byte>()
                    if srcOff &&& 0xFF <> 0 then op <- op ||| 0x01; ob.Add(byte (srcOff &&& 0xFF))
                    if (srcOff >>> 8) &&& 0xFF <> 0 then op <- op ||| 0x02; ob.Add(byte ((srcOff >>> 8) &&& 0xFF))
                    if (srcOff >>> 16) &&& 0xFF <> 0 then op <- op ||| 0x04; ob.Add(byte ((srcOff >>> 16) &&& 0xFF))
                    if (srcOff >>> 24) &&& 0xFF <> 0 then op <- op ||| 0x08; ob.Add(byte ((srcOff >>> 24) &&& 0xFF))
                    if chunk &&& 0xFF <> 0 then op <- op ||| 0x10; ob.Add(byte (chunk &&& 0xFF))
                    if (chunk >>> 8) &&& 0xFF <> 0 then op <- op ||| 0x20; ob.Add(byte ((chunk >>> 8) &&& 0xFF))
                    if (chunk >>> 16) &&& 0xFF <> 0 then op <- op ||| 0x40; ob.Add(byte ((chunk >>> 16) &&& 0xFF))
                    out.Add(byte op)
                    out.AddRange ob
                    srcOff <- srcOff + chunk
                    remaining <- remaining - chunk
                i <- i + matchLen
            else
                literals.Add t.[i]
                i <- i + 1
        flush ()
        out.ToArray()

    // ---- pack + index writing --------------------------------------------

    /// Build a packfile for the given object ids. With useDelta, same-type objects
    /// are delta-chained (ofs-delta, kept only when smaller). Returns the pack
    /// bytes and per-object (hash, offset, crc32) for the index.
    let buildPack (repo: Repo) (objectIds: GitHash[]) (useDelta: bool) : byte[] * (GitHash * int * uint32)[] =
        let objs =
            objectIds
            |> Array.choose (fun h ->
                match ReadObjects.readRawObject repo h with
                | Ok (t, c) -> Some(h, t, c)
                | Error _ -> None)
        let ordered =
            if useDelta then objs |> Array.sortBy (fun (_, t, c) -> (PackData.typeNum t, -c.Length))
            else objs
        use ms = new MemoryStream()
        ms.Write("PACK"B, 0, 4)
        ms.Write(be32 2, 0, 4)
        ms.Write(be32 ordered.Length, 0, 4)
        let offsets = ResizeArray<GitHash * int>()
        let written = Dictionary<GitHash, int * byte[] * int>() // hash -> (offset, content, depth)
        let prevByType = Dictionary<int, GitHash>()
        for (h, t, content) in ordered do
            let entryStart = int ms.Position
            let mutable wroteDelta = false
            if useDelta then
                match prevByType.TryGetValue(PackData.typeNum t) with
                | true, baseHash ->
                    let (baseOff, baseContent, baseDepth) = written.[baseHash]
                    if baseDepth < 50 then
                        let delta = encodeDelta baseContent content
                        let zd = Compression.compress delta
                        let zc = Compression.compress content
                        if zd.Length < zc.Length then
                            writeObjHeaderTo ms 6 delta.Length
                            writeOfsDistance ms (entryStart - baseOff)
                            ms.Write(zd, 0, zd.Length)
                            written.[h] <- (entryStart, content, baseDepth + 1)
                            wroteDelta <- true
                | _ -> ()
            if not wroteDelta then
                writeObjHeaderTo ms (PackData.typeNum t) content.Length
                let zc = Compression.compress content
                ms.Write(zc, 0, zc.Length)
                written.[h] <- (entryStart, content, 0)
            prevByType.[PackData.typeNum t] <- h
            offsets.Add(h, entryStart)
        let body = ms.ToArray()
        let entries =
            offsets
            |> Seq.mapi (fun i (h, off) ->
                let nextOff = if i + 1 < offsets.Count then snd offsets.[i + 1] else body.Length
                (h, off, crc32 body.[off .. nextOff - 1]))
            |> Seq.toArray
        let checksum = SHA1.HashData body
        Array.append body checksum, entries

    /// Build a v2 packfile index for the given entries (and the pack's trailer sha).
    let writeIdx (entries: (GitHash * int * uint32)[]) (packSha: byte[]) : byte[] =
        let sorted = entries |> Array.sortBy (fun (h, _, _) -> h)
        let firstByte (h: GitHash) = int (Convert.ToByte(h.Substring(0, 2), 16))
        let counts = Array.zeroCreate 256
        for (h, _, _) in sorted do counts.[firstByte h] <- counts.[firstByte h] + 1
        let fan = Array.zeroCreate 256
        let mutable acc = 0
        for i in 0 .. 255 do
            acc <- acc + counts.[i]
            fan.[i] <- acc
        use ms = new MemoryStream()
        ms.Write([| 0xFFuy; 0x74uy; 0x4Fuy; 0x63uy |], 0, 4) // \377tOc
        ms.Write(be32 2, 0, 4)
        for i in 0 .. 255 do ms.Write(be32 fan.[i], 0, 4)
        for (h, _, _) in sorted do let hb = hexToBytes h in ms.Write(hb, 0, 20)
        for (_, _, crc) in sorted do ms.Write(beU32 crc, 0, 4)
        for (_, off, _) in sorted do ms.Write(be32 off, 0, 4) // assumes pack < 2GiB
        ms.Write(packSha, 0, 20)
        let idxBody = ms.ToArray()
        Array.append idxBody (SHA1.HashData idxBody)

    // ---- gc / repack ------------------------------------------------------

    let private looseHashes (repo: Repo) : GitHash list =
        let od = Repository.getObjectsDir repo
        if not (Directory.Exists od) then []
        else
            [ for sub in Directory.GetDirectories od do
                let name = Path.GetFileName sub
                if name.Length = 2 then
                    for f in Directory.GetFiles sub do
                        yield name + Path.GetFileName f ]

    let private packedHashes (repo: Repo) : GitHash list =
        [ for (name, _) in Repository.packFilesExist repo do
            match PackParser.readPackIndex (Repository.getPackIndexPath repo name) with
            | Ok idx -> yield! (idx.Objects |> Array.map (fun o -> o.Hash))
            | Error _ -> () ]

    /// Collapse all loose objects and existing packs into a single delta-compressed
    /// packfile, then delete the loose objects and superseded packs. Returns the
    /// number of objects packed.
    let repack (repo: Repo) : Result<int, string> =
        try
            let all = (looseHashes repo @ packedHashes repo) |> List.distinct |> List.toArray
            if all.Length = 0 then Ok 0
            else
                let pack, entries = buildPack repo all true
                let packSha = pack.[pack.Length - 20 ..]
                let idx = writeIdx entries packSha
                let nameHex = BitConverter.ToString(packSha).Replace("-", "").ToLowerInvariant()
                let packName = "pack-" + nameHex
                let packDir = Repository.getPackDir repo
                Directory.CreateDirectory packDir |> ignore
                File.WriteAllBytes(Path.Combine(packDir, packName + ".pack"), pack)
                File.WriteAllBytes(Path.Combine(packDir, packName + ".idx"), idx)
                // delete superseded packs (everything but the new one)
                for f in Directory.GetFiles(packDir) do
                    let bn = Path.GetFileNameWithoutExtension f
                    if bn <> packName then File.Delete f
                // delete loose objects (now all in the pack)
                let od = Repository.getObjectsDir repo
                for sub in Directory.GetDirectories od do
                    if (Path.GetFileName sub).Length = 2 then
                        for f in Directory.GetFiles sub do File.Delete f
                        try Directory.Delete sub with _ -> ()
                Ok all.Length
        with ex -> Error $"repack failed: {ex.Message}"
