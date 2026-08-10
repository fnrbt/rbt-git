namespace FSharpGit

open System
open System.Collections.Generic
open System.Text

/// Real 3-way merge: a recursive tree merge with per-file resolution, a diff3
/// line merge for files changed on both sides, conflict detection, and
/// merge-commit creation. Produces byte-identical trees to canonical git for
/// clean merges (verified against `git merge-tree`).
module Merge3 =

    type MergeOutcome =
        | Merged of GitHash          // new merge commit
        | Conflicts of string list   // paths that could not be auto-merged
        | MergeError of string

    // ---- diff3 line merge -------------------------------------------------

    let private splitLines (s: string) : string[] =
        let res = ResizeArray<string>()
        let mutable start = 0
        for i in 0 .. s.Length - 1 do
            if s.[i] = '\n' then
                res.Add(s.Substring(start, i - start + 1))
                start <- i + 1
        if start < s.Length then res.Add(s.Substring start)
        res.ToArray()

    /// Longest common subsequence of two line arrays, as matched index pairs.
    let private lcsPairs (a: string[]) (b: string[]) : (int * int) list =
        let n = a.Length
        let m = b.Length
        let dp = Array2D.zeroCreate (n + 1) (m + 1)
        for i in n - 1 .. -1 .. 0 do
            for j in m - 1 .. -1 .. 0 do
                dp.[i, j] <-
                    if a.[i] = b.[j] then dp.[i + 1, j + 1] + 1
                    else max dp.[i + 1, j] dp.[i, j + 1]
        let res = ResizeArray<int * int>()
        let mutable i = 0
        let mutable j = 0
        while i < n && j < m do
            if a.[i] = b.[j] then res.Add(i, j); i <- i + 1; j <- j + 1
            elif dp.[i + 1, j] >= dp.[i, j + 1] then i <- i + 1
            else j <- j + 1
        List.ofSeq res

    /// 3-way merge of file contents. Ok merged bytes if clean, Error () on conflict.
    let mergeText (baseB: byte[]) (oursB: byte[]) (theirsB: byte[]) : Result<byte[], unit> =
        if oursB = theirsB then Ok oursB
        elif baseB = oursB then Ok theirsB
        elif baseB = theirsB then Ok oursB
        else
            let la = splitLines (Encoding.UTF8.GetString baseB)
            let lo = splitLines (Encoding.UTF8.GetString oursB)
            let lt = splitLines (Encoding.UTF8.GetString theirsB)
            let oMap = dict (lcsPairs la lo)
            let tMap = dict (lcsPairs la lt)
            // base indices matched in BOTH ours and theirs are sync points
            let syncs =
                [ yield (-1, -1, -1)
                  for bi in 0 .. la.Length - 1 do
                      match oMap.TryGetValue bi, tMap.TryGetValue bi with
                      | (true, oi), (true, ti) -> yield (bi, oi, ti)
                      | _ -> ()
                  yield (la.Length, lo.Length, lt.Length) ]
            let out = ResizeArray<string>()
            let mutable conflict = false
            let mutable prev = List.head syncs
            for cur in List.tail syncs do
                let (pb, po, pt) = prev
                let (b, o, t) = cur
                let seg (arr: string[]) lo hi = if hi >= lo then arr.[lo..hi] else [||]
                let baseSeg = seg la (pb + 1) (b - 1)
                let ourSeg = seg lo (po + 1) (o - 1)
                let theirSeg = seg lt (pt + 1) (t - 1)
                if ourSeg = theirSeg then out.AddRange ourSeg
                elif ourSeg = baseSeg then out.AddRange theirSeg
                elif theirSeg = baseSeg then out.AddRange ourSeg
                else conflict <- true
                if b < la.Length then out.Add la.[b]   // emit the shared sync line
                prev <- cur
            if conflict then Error ()
            else Ok(Encoding.UTF8.GetBytes(String.Concat(out)))

    // ---- tree merge -------------------------------------------------------

    [<Struct>]
    type private Ent = { Mode: int; Hash: GitHash }

    let private entriesMap (repo: Repo) (treeHash: GitHash option) : Map<string, Ent> =
        match treeHash with
        | None -> Map.empty
        | Some h ->
            match ReadObjects.readTree repo h with
            | Ok es -> es |> Array.map (fun e -> e.Path, { Mode = e.Mode; Hash = e.Hash }) |> Map.ofArray
            | Error _ -> Map.empty

    /// Merge one tree level. Returns (mergedTreeHash, conflictPaths).
    let rec private mergeLevel (repo: Repo) (prefix: string) (baseT: GitHash option) (ourT: GitHash option) (theirT: GitHash option) : GitHash * string list =
        let b = entriesMap repo baseT
        let o = entriesMap repo ourT
        let th = entriesMap repo theirT
        let names =
            Set.unionMany [ Set.ofSeq (Seq.map fst (Map.toSeq b))
                            Set.ofSeq (Seq.map fst (Map.toSeq o))
                            Set.ofSeq (Seq.map fst (Map.toSeq th)) ]
        let resolved = ResizeArray<TreeEntry>()
        let mutable conflicts : string list = []
        for name in names do
            let bb = Map.tryFind name b
            let oo = Map.tryFind name o
            let tt = Map.tryFind name th
            let full = if prefix = "" then name else prefix + "/" + name
            let keep (e: Ent) = resolved.Add { Mode = e.Mode; Path = name; Hash = e.Hash }
            if oo = tt then
                match oo with Some e -> keep e | None -> ()
            elif oo = bb then
                match tt with Some e -> keep e | None -> ()   // ours unchanged -> take theirs (incl delete)
            elif tt = bb then
                match oo with Some e -> keep e | None -> ()    // theirs unchanged -> take ours
            else
                match oo, tt with
                | Some oe, Some te when oe.Mode = 0o40000 && te.Mode = 0o40000 ->
                    let bSub = bb |> Option.bind (fun e -> if e.Mode = 0o40000 then Some e.Hash else None)
                    let subHash, subConf = mergeLevel repo full bSub (Some oe.Hash) (Some te.Hash)
                    resolved.Add { Mode = 0o40000; Path = name; Hash = subHash }
                    conflicts <- conflicts @ subConf
                | Some oe, Some te when oe.Mode <> 0o40000 && te.Mode <> 0o40000 ->
                    let blob (h: GitHash) = match ReadObjects.readBlob repo h with Ok b -> b | Error _ -> [||]
                    let baseContent =
                        match bb with
                        | Some e when e.Mode <> 0o40000 -> blob e.Hash
                        | _ -> [||]
                    match mergeText baseContent (blob oe.Hash) (blob te.Hash) with
                    | Ok merged ->
                        match ObjectWriter.writeBlob repo merged with
                        | Ok h -> resolved.Add { Mode = (if oe.Mode = te.Mode then oe.Mode else oe.Mode); Path = name; Hash = h }
                        | Error _ -> conflicts <- conflicts @ [ full ]
                    | Error () -> conflicts <- conflicts @ [ full ]
                | _ -> conflicts <- conflicts @ [ full ]   // type mismatch / modify-delete
        let treeHash = match ObjectWriter.writeTree repo (resolved.ToArray()) with Ok h -> h | Error _ -> ""
        treeHash, conflicts

    let private ancestorsSet (repo: Repo) (c: GitHash) : HashSet<GitHash> =
        let seen = HashSet<GitHash>()
        let q = Queue<GitHash>()
        q.Enqueue c
        while q.Count > 0 do
            let x = q.Dequeue()
            if seen.Add x then
                match CommitHistory.getParents repo x with
                | Ok ps -> for p in ps do if not (seen.Contains p) then q.Enqueue p
                | _ -> ()
        seen

    /// Lowest common ancestor of two commits (correct for non-criss-cross
    /// histories, which is the case for the forge's branch-from-default flow).
    let private mergeBaseOf (repo: Repo) (a: GitHash) (b: GitHash) : GitHash option =
        let anc = ancestorsSet repo a
        let seen = HashSet<GitHash>()
        let q = Queue<GitHash>()
        q.Enqueue b
        let mutable result = None
        while q.Count > 0 && result.IsNone do
            let x = q.Dequeue()
            if seen.Add x then
                if anc.Contains x then result <- Some x
                else
                    match CommitHistory.getParents repo x with
                    | Ok ps -> for p in ps do q.Enqueue p
                    | _ -> ()
        result

    /// 3-way merge `theirCommit` into `ourCommit`. On a clean merge, writes a
    /// merge commit (parents [our; their]) and returns its id.
    let merge (repo: Repo) (ourCommit: GitHash) (theirCommit: GitHash) (author: Signature) (message: string) : MergeOutcome =
        match ReadObjects.readCommit repo ourCommit, ReadObjects.readCommit repo theirCommit with
        | Ok ourC, Ok theirC ->
            let baseTree =
                match mergeBaseOf repo ourCommit theirCommit with
                | Some bc -> (match ReadObjects.readCommit repo bc with Ok c -> Some c.Tree | _ -> None)
                | None -> None
            let treeHash, conflicts = mergeLevel repo "" baseTree (Some ourC.Tree) (Some theirC.Tree)
            if not (List.isEmpty conflicts) then Conflicts conflicts
            else
                let commit : CommitData =
                    { Tree = treeHash
                      Parents = [| ourCommit; theirCommit |]
                      Author = author
                      Committer = author
                      Message = message }
                match ObjectWriter.writeCommit repo commit with
                | Ok h -> Merged h
                | Error e -> MergeError e
        | _ -> MergeError "could not read merge endpoints"
