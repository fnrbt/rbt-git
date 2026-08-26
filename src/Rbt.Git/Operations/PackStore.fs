namespace Rbt.Git

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO

/// Correct random-access reader for objects stored in packfiles. Resolves
/// ofs-deltas (within the pack) and ref-deltas (within the pack, or against
/// loose objects / other packs for thin packs). Replaces the broken
/// PackParser.findPackObject read path. Pack files are immutable once written
/// (gc creates a uniquely named pack), so parsed indexes are cached by path.
/// Pack contents remain on disk and are read through a bounded FileStream.
module PackStore =

    // packPath -> hash -> offset
    let private cache = ConcurrentDictionary<string, Dictionary<GitHash, int64>>()

    let private loadIndex (packPath: string) (idxPath: string) : Dictionary<GitHash, int64> =
        cache.GetOrAdd(packPath, fun _ ->
            let map = Dictionary<GitHash, int64>()
            match PackParser.readPackIndex idxPath with
            | Ok idx -> for o in idx.Objects do map.[o.Hash] <- o.Offset
            | Error _ -> ()
            map)

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
            let map = loadIndex packPath idxPath
            match map.TryGetValue hash with
            | true, off ->
                use pack =
                    new FileStream(
                        packPath,
                        FileMode.Open,
                        FileAccess.Read,
                        FileShare.Read,
                        64 * 1024,
                        FileOptions.RandomAccess)
                // Resolve bases: ofs-delta by offset within THIS pack; ref-delta by
                // hash within this pack, else loose / other packs (thin packs).
                let rec byOff (o: int64) : (string * byte[]) option =
                    Some(PackData.readObjectAtStream pack o byOff byHash)
                and byHash (h: GitHash) : (string * byte[]) option =
                    match map.TryGetValue h with
                    | true, o2 -> byOff o2
                    | _ ->
                        match tryLoose repo h with
                        | Some r -> Some r
                        | None -> tryRead repo h
                result <- Some(PackData.readObjectAtStream pack off byOff byHash)
            | _ -> ()
            i <- i + 1
        result

    let exists (repo: Repo) (hash: GitHash) : bool =
        (tryRead repo hash).IsSome
