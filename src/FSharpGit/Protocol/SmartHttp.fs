namespace FSharpGit

open System
open System.IO
open System.Text

/// Git "smart HTTP" protocol (v0/v1), the transport `git clone`/`fetch`/`push`
/// speak over HTTP. These functions are pure over byte buffers — the host
/// (e.g. a RedBat route) is responsible for HTTP plumbing and content types:
///   info/refs           -> application/x-git-<service>-advertisement
///   git-upload-pack      -> application/x-git-upload-pack-result
///   git-receive-pack     -> application/x-git-receive-pack-result
module SmartHttp =

    let private zeroId = String('0', 40)

    let private w (ms: MemoryStream) (s: string) =
        let b = PktLine.encodeStr s
        ms.Write(b, 0, b.Length)

    /// Resolve a FULL ref name (e.g. "refs/heads/main" or "HEAD") to a sha,
    /// following symref chains and consulting packed-refs. (fsgit's
    /// References.resolveReference expects a refs-relative path and double-counts
    /// the "refs/" prefix, so we resolve against GitDir directly here.)
    let rec resolveFull (repo: Repo) (name: string) (depth: int) : GitHash option =
        if depth > 10 then None
        else
            let path = Path.Combine(repo.GitDir, name)
            if File.Exists path then
                let c = (File.ReadAllText path).Trim()
                if c.StartsWith "ref: " then resolveFull repo (c.Substring(5).Trim()) (depth + 1)
                else Some c
            else
                match References.readPackedRefs repo with
                | Ok arr -> arr |> Array.tryFind (fun (n, _) -> n = name) |> Option.map snd
                | _ -> None

    /// Resolved (sha, refname) for every advertised head and tag.
    let private gatherRefs (repo: Repo) : (GitHash * string) list =
        let refs = ResizeArray<GitHash * string>()
        match References.listBranches repo with
        | Ok bs ->
            for b in bs do
                match resolveFull repo ("refs/heads/" + b) 0 with
                | Some h -> refs.Add(h, "refs/heads/" + b)
                | None -> ()
        | _ -> ()
        match References.listTags repo with
        | Ok ts ->
            for t in ts do
                match resolveFull repo ("refs/tags/" + t) 0 with
                | Some h -> refs.Add(h, "refs/tags/" + t)
                | None -> ()
        | _ -> ()
        refs |> List.ofSeq |> List.sortBy snd

    /// Body of GET /info/refs?service=<service>.
    let advertiseRefs (repo: Repo) (service: string) : byte[] =
        use ms = new MemoryStream()
        w ms (sprintf "# service=%s\n" service)
        PktLine.writeFlush ms

        let refs = gatherRefs repo
        let headTarget =
            match References.readHead repo with
            | Ok (Symbolic target) -> Some target
            | _ -> None
        let baseCaps =
            match service with
            | "git-upload-pack" -> "shallow ofs-delta side-band-64k agent=fsgit/0.1"
            | _ -> "report-status delete-refs ofs-delta agent=fsgit/0.1"
        let caps =
            match service, headTarget with
            | "git-upload-pack", Some t -> sprintf "%s symref=HEAD:%s" baseCaps t
            | _ -> baseCaps

        // Advertise HEAD first for upload-pack so the client picks the default branch.
        let entries =
            match service, headTarget with
            | "git-upload-pack", Some t ->
                match resolveFull repo t 0 with
                | Some h -> (h, "HEAD") :: refs
                | None -> refs
            | _ -> refs

        match entries with
        | [] ->
            // No refs yet: advertise the zero id with capabilities^{}.
            w ms (sprintf "%s capabilities^{}\000%s\n" zeroId caps)
        | (sha0, name0) :: rest ->
            w ms (sprintf "%s %s\000%s\n" sha0 name0 caps)
            for (sha, name) in rest do
                w ms (sprintf "%s %s\n" sha name)
        PktLine.writeFlush ms
        ms.ToArray()

    /// Stream the body of POST /git-upload-pack (clone/fetch) directly to `output`:
    /// the shallow boundary (when deepen), "NAK", then the packfile (side-band
    /// framed when negotiated). Streaming keeps memory bounded for large repos.
    let uploadPackTo (repo: Repo) (reqBody: byte[]) (output: Stream) : Result<unit, string> =
        let wants = ResizeArray<GitHash>()
        let haves = ResizeArray<GitHash>()
        let mutable caps = ""
        let mutable depth = 0
        let mutable p = 0
        let mutable doneSeen = false
        while p < reqBody.Length && not doneSeen do
            match PktLine.read reqBody p with
            | PktLine.Flush, np | PktLine.Delim, np -> p <- np
            | PktLine.Data d, np ->
                p <- np
                let s = Encoding.UTF8.GetString d
                if s.StartsWith "want " && s.Length >= 45 then
                    wants.Add(s.Substring(5, 40))
                    if caps = "" && s.Length > 45 then caps <- s.Substring(45)
                elif s.StartsWith "have " && s.Length >= 45 then haves.Add(s.Substring(5, 40))
                elif s.StartsWith "deepen " then
                    match Int32.TryParse(s.Substring(7).Trim()) with
                    | true, n -> depth <- n
                    | _ -> ()
                elif s.StartsWith "done" then doneSeen <- true
        if wants.Count = 0 then Error "no wants in upload-pack request"
        else
            let useDelta = caps.Contains "ofs-delta"
            let useSideband = caps.Contains "side-band-64k"
            if depth > 0 then
                // Shallow negotiation is two POSTs. POST 1 (wants+deepen+flush, no
                // "done") gets only the shallow boundary; POST 2 (adds "done") gets
                // the shallow boundary + NAK + the depth-limited pack.
                let objs, shallowSet = PackWriter.objectClosureShallow repo (wants.ToArray()) depth
                for sh in shallowSet do PktLine.writeStr output (sprintf "shallow %s\n" sh)
                PktLine.writeFlush output
                if doneSeen then
                    PktLine.writeStr output "NAK\n"
                    PackStream.writeTo repo objs useDelta useSideband output
                Ok()
            else
                let objs = PackWriter.objectClosure repo (wants.ToArray()) (haves.ToArray())
                PktLine.writeStr output "NAK\n"
                PackStream.writeTo repo objs useDelta useSideband output
                Ok()

    /// Buffered convenience wrapper (used by tests); the route streams via uploadPackTo.
    let uploadPack (repo: Repo) (reqBody: byte[]) : Result<byte[], string> =
        use ms = new MemoryStream()
        match uploadPackTo repo reqBody ms with
        | Ok () -> Ok(ms.ToArray())
        | Error e -> Error e

    /// Push policy. ProtectedRefs may not be force-updated or deleted (typically
    /// the default branch). AllowNonFastForward governs force-push on other refs.
    type ReceiveOptions = {
        ProtectedRefs: string list
        AllowNonFastForward: bool
    }

    let defaultReceiveOptions = { ProtectedRefs = []; AllowNonFastForward = true }

    // One lock per repo (keyed by git dir) serializes pushes to a repo so
    // concurrent agents can't interleave unpack + ref updates.
    let private repoLocks = System.Collections.Concurrent.ConcurrentDictionary<string, obj>()
    let private lockFor (repo: Repo) : obj = repoLocks.GetOrAdd(repo.GitDir, fun _ -> obj ())

    /// Streaming POST /git-receive-pack (push): reads the ref-update commands then
    /// stream-unpacks the packfile straight off `input` (bounded memory), applies
    /// the policy (CAS, non-fast-forward + delete protection on protected refs),
    /// and writes report-status to `output`.
    let receivePackStreamWith (repo: Repo) (options: ReceiveOptions) (input: Stream) (output: Stream) : unit =
        let buf = new BufferedStream(input)
        // 1. command list, terminated by a flush
        let commands = ResizeArray<string * string * string>()
        let mutable flushed = false
        while not flushed do
            match PktLine.readFrom buf with
            | Some (PktLine.Data d) ->
                let raw = Encoding.UTF8.GetString d
                let line = (let i = raw.IndexOf('\000') in if i >= 0 then raw.Substring(0, i) else raw).TrimEnd('\n')
                let parts = line.Split(' ')
                if parts.Length >= 3 then commands.Add(parts.[0], parts.[1], parts.[2])
            | Some PktLine.Delim -> ()
            | Some PktLine.Flush -> flushed <- true
            | None -> flushed <- true

        lock (lockFor repo) (fun () ->
            // 2. stream-unpack the packfile (bounded memory)
            let counting = new PackData.CountingStream(buf)
            let mutable unpackStatus = "ok"
            match PackWriter.unpackPackStream repo counting with
            | Ok _ -> ()
            | Error e -> unpackStatus <- e

            // 3. validate + apply each ref command under policy
            let isProtected (ref: string) = List.contains ref options.ProtectedRefs
            let results = ResizeArray<string * string>() // (refname, "ok" | "ng <reason>")
            for (oldSha, newSha, refName) in commands do
                let current = resolveFull repo refName 0 |> Option.defaultValue zeroId
                if unpackStatus <> "ok" then
                    results.Add(refName, "ng unpacker error")
                elif current <> oldSha then
                    // someone else moved the ref since the client last saw it
                    results.Add(refName, "ng fetch first")
                elif newSha = zeroId then
                    if isProtected refName then results.Add(refName, "ng protected ref")
                    else
                        try
                            let path = Path.Combine(repo.GitDir, refName)
                            if File.Exists path then File.Delete path
                            results.Add(refName, "ok")
                        with ex -> results.Add(refName, "ng " + ex.Message)
                else
                    let isFF =
                        oldSha = zeroId
                        || (match CommitHistory.isAncestor repo oldSha newSha with Ok true -> true | _ -> false)
                    if (not isFF) && (isProtected refName || not options.AllowNonFastForward) then
                        results.Add(refName, "ng non-fast-forward")
                    else
                        try
                            let path = Path.Combine(repo.GitDir, refName)
                            Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
                            File.WriteAllText(path, newSha + "\n")
                            results.Add(refName, "ok")
                        with ex -> results.Add(refName, "ng " + ex.Message)

            // If HEAD is unborn (points at a missing ref) and exactly one branch
            // now exists, repoint HEAD so clones check out something sensible.
            (match References.readHead repo with
             | Ok (Symbolic t) when not (File.Exists(Path.Combine(repo.GitDir, t))) ->
                 match References.listBranches repo with
                 | Ok [| only |] -> References.updateHead repo (Symbolic("refs/heads/" + only)) |> ignore
                 | _ -> ()
             | _ -> ())

            // 4. report-status
            PktLine.writeStr output (sprintf "unpack %s\n" unpackStatus)
            for (refName, st) in results do
                if st = "ok" then PktLine.writeStr output (sprintf "ok %s\n" refName)
                else PktLine.writeStr output (sprintf "ng %s %s\n" refName (st.Substring 3))
            PktLine.writeFlush output
            output.Flush())

    let receivePackStream (repo: Repo) (input: Stream) (output: Stream) : unit =
        receivePackStreamWith repo defaultReceiveOptions input output

    /// Buffered convenience wrapper (used by tests); the route streams via receivePackStreamWith.
    let receivePackWith (repo: Repo) (options: ReceiveOptions) (reqBody: byte[]) : byte[] =
        use input = new MemoryStream(reqBody)
        use output = new MemoryStream()
        receivePackStreamWith repo options input output
        output.ToArray()

    /// Back-compat: receive with the permissive default policy.
    let receivePack (repo: Repo) (reqBody: byte[]) : byte[] =
        receivePackWith repo defaultReceiveOptions reqBody

    // ======================================================================
    // Protocol v2 (negotiated via the "Git-Protocol: version=2" HTTP header).
    // Push (receive-pack) stays v0; only fetch/clone use v2.
    // ======================================================================

    /// v2 capability advertisement for GET /info/refs (version=2).
    let advertiseRefsV2 (repo: Repo) : byte[] =
        use ms = new MemoryStream()
        w ms "# service=git-upload-pack\n"
        PktLine.writeFlush ms
        w ms "version 2\n"
        w ms "agent=fsgit/0.1\n"
        w ms "ls-refs=unborn\n"
        w ms "fetch=shallow\n"
        w ms "object-format=sha1\n"
        PktLine.writeFlush ms
        ms.ToArray()

    /// Parse a v2 command request into (command, args). Capability lines (between
    /// the command line and the delimiter) are ignored; args follow the delimiter.
    let private parseV2Request (reqBody: byte[]) : string * string list =
        let mutable p = 0
        let mutable command = ""
        let args = ResizeArray<string>()
        let mutable afterDelim = false
        let mutable stop = false
        while p < reqBody.Length && not stop do
            match PktLine.read reqBody p with
            | PktLine.Flush, np -> p <- np; stop <- true
            | PktLine.Delim, np -> p <- np; afterDelim <- true
            | PktLine.Data d, np ->
                p <- np
                let s = (Encoding.UTF8.GetString d).TrimEnd('\n')
                if s.StartsWith "command=" then command <- s.Substring 8
                elif afterDelim then args.Add s
        command, List.ofSeq args

    let private lsRefsV2 (repo: Repo) (args: string list) : byte[] =
        let wantSymrefs = List.contains "symrefs" args
        let wantPeel = List.contains "peel" args
        let wantUnborn = List.contains "unborn" args
        let prefixes = args |> List.choose (fun a -> if a.StartsWith "ref-prefix " then Some(a.Substring 11) else None)
        let matches (name: string) = List.isEmpty prefixes || prefixes |> List.exists (fun pre -> name.StartsWith pre)
        use ms = new MemoryStream()
        (match References.readHead repo with
         | Ok (Symbolic target) when matches "HEAD" ->
             match resolveFull repo target 0 with
             | Some sha ->
                 let line = sprintf "%s HEAD" sha
                 w ms ((if wantSymrefs then line + sprintf " symref-target:%s" target else line) + "\n")
             | None when wantUnborn ->
                 let line = "unborn HEAD"
                 w ms ((if wantSymrefs then line + sprintf " symref-target:%s" target else line) + "\n")
             | None -> ()
         | _ -> ())
        for (sha, name) in gatherRefs repo do
            if matches name then
                let mutable line = sprintf "%s %s" sha name
                if wantPeel then
                    match ReadObjects.readObject repo sha with
                    | Ok (GitObject.Tag t) -> line <- line + sprintf " peeled:%s" t.Object
                    | _ -> ()
                w ms (line + "\n")
        PktLine.writeFlush ms
        ms.ToArray()

    let private fetchV2To (repo: Repo) (args: string list) (output: Stream) : unit =
        let pick (prefix: string) =
            args |> List.choose (fun a -> if a.StartsWith prefix then Some(a.Substring(prefix.Length).Trim()) else None)
        let wants = pick "want " |> List.map (fun s -> s.Substring(0, 40)) |> List.toArray
        let haves = pick "have " |> List.map (fun s -> s.Substring(0, 40))
        let doneSeen = List.contains "done" args
        let useDelta = List.contains "ofs-delta" args
        let depth =
            args
            |> List.tryPick (fun a ->
                if a.StartsWith "deepen " then (match Int32.TryParse(a.Substring(7).Trim()) with | true, n -> Some n | _ -> None)
                else None)
            |> Option.defaultValue 0
        let clientShallow = pick "shallow " |> Set.ofList

        let writeShallowInfo () =
            if depth > 0 then
                let _, shallowSet = PackWriter.objectClosureShallow repo wants depth
                PktLine.writeStr output "shallow-info\n"
                for sh in shallowSet do
                    if not (clientShallow.Contains sh) then PktLine.writeStr output (sprintf "shallow %s\n" sh)
                PktLine.writeDelim output

        let writePackfile () =
            PktLine.writeStr output "packfile\n"
            let objs =
                if depth > 0 then fst (PackWriter.objectClosureShallow repo wants depth)
                else PackWriter.objectClosure repo wants (List.toArray haves)
            // v2 packfile data is always side-band framed; PackStream writes the
            // terminating flush-pkt that ends the whole response.
            PackStream.writeTo repo objs useDelta true output

        if doneSeen || List.isEmpty haves then
            writeShallowInfo ()
            writePackfile ()
        else
            let common = haves |> List.filter (ReadObjects.objectExists repo)
            PktLine.writeStr output "acknowledgments\n"
            if List.isEmpty common then
                PktLine.writeStr output "NAK\n"
                PktLine.writeFlush output
            else
                for h in common do PktLine.writeStr output (sprintf "ACK %s\n" h)
                PktLine.writeStr output "ready\n"
                PktLine.writeDelim output
                writeShallowInfo ()
                writePackfile ()

    /// Dispatch a v2 command POST (ls-refs / fetch), streaming to `output`.
    let uploadPackV2To (repo: Repo) (reqBody: byte[]) (output: Stream) : Result<unit, string> =
        match parseV2Request reqBody with
        | "ls-refs", args ->
            let b = lsRefsV2 repo args
            output.Write(b, 0, b.Length)
            Ok()
        | "fetch", args ->
            fetchV2To repo args output
            Ok()
        | other, _ -> Error(sprintf "unsupported v2 command: %s" other)
