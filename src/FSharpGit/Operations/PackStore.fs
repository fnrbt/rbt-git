namespace FSharpGit

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO

/// Correct random-access reader for objects stored in packfiles. Resolves
/// ofs-deltas (within the pack) and ref-deltas (within the pack, or against
/// loose objects / other packs for thin packs). Replaces the broken
/// PackParser.findPackObject read path. Pack files are immutable once written
/// (gc creates a uniquely named pack), so loaded packs are cached by path.
module PackStore =

    // packPath -> (packBytes, hash -> offset)
    let private cache = ConcurrentDictionary<string, byte[] * Dictionary<GitHash, int>>()

    let private loadPack (packPath: string) (idxPath: string) : byte[] * Dictionary<GitHash, int> =
        cache.GetOrAdd(packPath, fun _ ->
            let bytes = File.ReadAllBytes packPath
            let map = Dictionary<GitHash, int>()
            match PackParser.readPackIndex idxPath with
            | Ok idx -> for o in idx.Objects do map.[o.Hash] <- int o.Offset
            | Error _ -> ()
            bytes, map)

    let private tryLoose (repo: Repo) (hash: GitHash) : (string * byte[]) option =
        let path = Repository.getLooseObjectPath repo hash
        if File.Exists path then
            let dec = Compression.decompress (File.ReadAllBytes path)
            let nul = System.Array.IndexOf(dec, 0uy)
            if nul < 0 then None
            else
                let header = System.Text.Encoding.UTF8.GetString(dec, 0, nul)
                let sp = header.IndexOf ' '
                let t = if sp < 0 then header else header.Substring(0, sp)
                Some(t, dec.[nul + 1 ..])
        else None

    /// Read an object by hash from any packfile in the repo. None if not packed.
    let rec tryRead (repo: Repo) (hash: GitHash) : (string * byte[]) option =
        let packs = Repository.packFilesExist repo
        let mutable result = None
        let mutable i = 0
        while result.IsNone && i < packs.Length do
            let (name, packPath) = packs.[i]
            let idxPath = Repository.getPackIndexPath repo name
            let bytes, map = loadPack packPath idxPath
            match map.TryGetValue hash with
            | true, off ->
                // Resolve bases: ofs-delta by offset within THIS pack; ref-delta by
                // hash within this pack, else loose / other packs (thin packs).
                let rec byOff (o: int) : (string * byte[]) option =
                    Some(PackData.readObjectAt bytes o byOff byHash)
                and byHash (h: GitHash) : (string * byte[]) option =
                    match map.TryGetValue h with
                    | true, o2 -> byOff o2
                    | _ ->
                        match tryLoose repo h with
                        | Some r -> Some r
                        | None -> tryRead repo h
                result <- Some(PackData.readObjectAt bytes off byOff byHash)
            | _ -> ()
            i <- i + 1
        result

    let exists (repo: Repo) (hash: GitHash) : bool =
        (tryRead repo hash).IsSome
