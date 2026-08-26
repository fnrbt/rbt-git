namespace Rbt.Git

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
    /// following symref chains and consulting packed-refs. (Rbt.Git's
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

    let private wantsAreAdvertised (repo: Repo) (wants: seq<GitHash>) =
        let advertised = System.Collections.Generic.HashSet<GitHash>(StringComparer.Ordinal)
        for hash, _ in gatherRefs repo do
            advertised.Add(hash) |> ignore
            match ReadObjects.readObject repo hash with
            | Ok (GitObject.Tag tag) -> advertised.Add(tag.Object) |> ignore
            | _ -> ()
        wants
        |> Seq.forall (fun hash -> Hashing.isValidHash hash && advertised.Contains hash)

    /// Ref advertisement for Git protocol v0/v1 without an HTTP service header.
    /// This is what direct transports such as SSH write immediately after exec.
    let private advertiseRefsRawWith (repo: Repo) (service: string) (refs: (GitHash * string) list) : byte[] =
        use ms = new MemoryStream()
        let headTarget =
            match References.readHead repo with
            | Ok (Symbolic target) -> Some target
            | _ -> None
        let baseCaps =
            match service with
            | "git-upload-pack" -> "shallow ofs-delta side-band-64k agent=rbt-git/0.1"
            | _ -> "report-status delete-refs ofs-delta agent=rbt-git/0.1"
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

    /// Ref advertisement for Git protocol v0/v1 without an HTTP service header.
    let advertiseRefsRaw (repo: Repo) (service: string) : byte[] =
        advertiseRefsRawWith repo service (gatherRefs repo)

    /// Body of GET /info/refs?service=<service>.
    let advertiseRefs (repo: Repo) (service: string) : byte[] =
        use ms = new MemoryStream()
        w ms (sprintf "# service=%s\n" service)
        PktLine.writeFlush ms
        let body = advertiseRefsRaw repo service
        ms.Write(body, 0, body.Length)
        ms.ToArray()

    /// Stream the body of POST /git-upload-pack (clone/fetch) directly to `output`:
    /// the shallow boundary (when deepen), "NAK", then the packfile (side-band
    /// framed when negotiated). Streaming keeps memory bounded for large repos.
    let private uploadPackToWithAuthorization
        (authorize: seq<GitHash> -> bool)
        (repo: Repo)
        (reqBody: byte[])
        (output: Stream) : Result<unit, string> =
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
        elif not (authorize wants) then Error "want is not reachable from an advertised ref"
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
            elif not doneSeen then
                // Stateless-HTTP compute round: reply "NAK" and end the response.
                // We advertise no multi_ack, so we never acknowledge a common --
                // git keeps offering haves and, receiving only NAK, sends "done"
                // and reads the final NAK + pack via get_pack(). Critically, emit
                // NO trailing flush: git's get_ack() reads exactly one ACK/NAK and
                // a following flush aborts the fetch with
                // "expected ACK/NAK, got a flush packet". (Sending "ACK <sha>"
                // instead would put git in its "ready" state, after which it
                // expects the done round to be the packfile alone.)
                PktLine.writeStr output "NAK\n"
                Ok()
            else
                // Done round: final NAK then the packfile. objectClosure excludes
                // history reachable from the haves, so the pack stays incremental
                // even though we never ACKed a specific common.
                let objs = PackWriter.objectClosure repo (wants.ToArray()) (haves.ToArray())
                PktLine.writeStr output "NAK\n"
                PackStream.writeTo repo objs useDelta useSideband output
                Ok()

    let uploadPackTo (repo: Repo) (reqBody: byte[]) (output: Stream) : Result<unit, string> =
        uploadPackToWithAuthorization (wantsAreAdvertised repo) repo reqBody output

    let advertiseRefsForPolicy (repo: Repo) (service: string) (policy: RefPolicy) : Result<byte[], string> =
        References.snapshot repo policy
        |> Result.map (fun snapshot ->
            use ms = new MemoryStream()
            w ms (sprintf "# service=%s\n" service)
            PktLine.writeFlush ms
            let refs = snapshot.Refs |> Map.toList |> List.map (fun (name, hash) -> hash, name)
            let body = advertiseRefsRawWith repo service refs
            ms.Write(body, 0, body.Length)
            ms.ToArray())

    let uploadPackToForPolicy
        (repo: Repo)
        (policy: RefPolicy)
        (reqBody: byte[])
        (output: Stream) : Result<unit, string> =
        match References.snapshot repo policy with
        | Error error -> Error error
        | Ok snapshot ->
            let allowed = System.Collections.Generic.HashSet<GitHash>(snapshot.Refs.Values, StringComparer.Ordinal)
            let authorize wants =
                wants
                |> Seq.forall (fun hash -> Hashing.isValidHash hash && allowed.Contains hash)
            uploadPackToWithAuthorization authorize repo reqBody output

    /// Buffered convenience wrapper (used by tests); the route streams via uploadPackTo.
    let uploadPack (repo: Repo) (reqBody: byte[]) : Result<byte[], string> =
        use ms = new MemoryStream()
        match uploadPackTo repo reqBody ms with
        | Ok () -> Ok(ms.ToArray())
        | Error e -> Error e

    /// Direct transport upload-pack (SSH/git:// style). Unlike smart HTTP, this
    /// can be interactive: the client sends wants, flushes, then may send haves
    /// in rounds before the final done. We advertise no multi_ack capability, so
    /// a simple NAK per negotiation flush is sufficient.
    let uploadPackStream (repo: Repo) (input: Stream) (output: Stream) : Result<unit, string> =
        let wants = ResizeArray<GitHash>()
        let haves = ResizeArray<GitHash>()
        let mutable caps = ""
        let mutable depth = 0
        let mutable doneSeen = false
        let mutable eof = false
        let mutable sentShallow = false
        let mutable sentNak = false
        let mutable invalidWant = false

        let shallowInfo () =
            if depth > 0 && not sentShallow then
                let _, shallowSet = PackWriter.objectClosureShallow repo (wants.ToArray()) depth
                for sh in shallowSet do PktLine.writeStr output (sprintf "shallow %s\n" sh)
                PktLine.writeFlush output
                output.Flush()
                sentShallow <- true

        while not doneSeen && not eof do
            match PktLine.readFrom input with
            | Some (PktLine.Data d) ->
                let s = Encoding.UTF8.GetString d
                if s.StartsWith "want " && s.Length >= 45 then
                    wants.Add(s.Substring(5, 40))
                    if caps = "" && s.Length > 45 then caps <- s.Substring(45)
                elif s.StartsWith "have " && s.Length >= 45 then
                    haves.Add(s.Substring(5, 40))
                elif s.StartsWith "deepen " then
                    match Int32.TryParse(s.Substring(7).Trim()) with
                    | true, n -> depth <- n
                    | _ -> ()
                elif s.StartsWith "done" then
                    doneSeen <- true
            | Some PktLine.Flush ->
                if wants.Count > 0 && not doneSeen then
                    if wantsAreAdvertised repo wants then
                        shallowInfo()
                        PktLine.writeStr output "NAK\n"
                        output.Flush()
                        sentNak <- true
                    else
                        invalidWant <- true
                        doneSeen <- true
            | Some PktLine.Delim -> ()
            | None -> eof <- true

        if wants.Count = 0 then Ok()
        elif invalidWant || not (wantsAreAdvertised repo wants) then
            Error "want is not reachable from an advertised ref"
        else
            shallowInfo()
            if not sentNak then PktLine.writeStr output "NAK\n"
            let useDelta = caps.Contains "ofs-delta"
            let useSideband = caps.Contains "side-band-64k"
            let objs =
                if depth > 0 then fst (PackWriter.objectClosureShallow repo (wants.ToArray()) depth)
                else PackWriter.objectClosure repo (wants.ToArray()) (haves.ToArray())
            PackStream.writeTo repo objs useDelta useSideband output
            Ok()

    /// Push policy. ProtectedRefs may not be force-updated or deleted (typically
    /// the default branch). AllowNonFastForward governs force-push on other refs;
    /// AllowProtectedRefRewrite is an explicit caller-authorized maintenance path.
    type ReceiveOptions = {
        ProtectedRefs: string list
        AllowNonFastForward: bool
        AllowProtectedRefRewrite: bool
    }

    let defaultReceiveOptions =
        { ProtectedRefs = []
          AllowNonFastForward = true
          AllowProtectedRefRewrite = false }

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
        let commands = ResizeArray<string * string * string>()
        let mutable flushed = false
        while not flushed do
            match PktLine.readFrom buf with
            | Some (PktLine.Data data) ->
                let raw = Encoding.UTF8.GetString data
                let line = (let index = raw.IndexOf('\000') in if index >= 0 then raw.Substring(0, index) else raw).TrimEnd('\n')
                match line.Split(' ', StringSplitOptions.RemoveEmptyEntries) with
                | [| oldSha; newSha; refName |] -> commands.Add(oldSha.ToLowerInvariant(), newSha.ToLowerInvariant(), refName)
                | _ -> ()
            | Some PktLine.Delim -> ()
            | Some PktLine.Flush -> flushed <- true
            | None -> flushed <- true

        lock (lockFor repo) (fun () ->
            let mutable unpackStatus = "ok"
            let needsPack = commands |> Seq.exists (fun (_, newSha, _) -> newSha <> zeroId)
            let mutable introducedObjects: GitHash[] = [||]
            let quarantinePath = repo.GitDir + ".quarantine-" + Guid.NewGuid().ToString("N")
            try
                if needsPack then
                    match Repository.initBare quarantinePath with
                    | Error error -> unpackStatus <- error
                    | Ok quarantine ->
                        use counting = new PackData.CountingStream(buf)
                        match PackWriter.unpackPackStreamInto repo quarantine counting with
                        | Error error -> unpackStatus <- error
                        | Ok written ->
                            match PackWriter.promoteLooseObjects quarantine repo written with
                            | Ok () -> introducedObjects <- written
                            | Error error -> unpackStatus <- error

                let isProtected refName = List.contains refName options.ProtectedRefs
                let results = ResizeArray<string * string>()
                for oldSha, newSha, refName in commands do
                    let validIds = Hashing.isValidHash oldSha && Hashing.isValidHash newSha
                    let current = resolveFull repo refName 0 |> Option.defaultValue zeroId
                    if unpackStatus <> "ok" then
                        results.Add(refName, "ng unpacker error")
                    elif not validIds || not (References.isValidName refName) then
                        results.Add(refName, "ng invalid ref command")
                    elif current <> oldSha then
                        results.Add(refName, "ng fetch first")
                    elif newSha = zeroId then
                        if isProtected refName then results.Add(refName, "ng protected ref")
                        else results.Add(refName, "ok")
                    elif not (ReadObjects.objectExists repo newSha) then
                        results.Add(refName, "ng missing object")
                    else
                        let isFastForward =
                            oldSha = zeroId
                            || (match CommitHistory.isAncestor repo oldSha newSha with Ok true -> true | _ -> false)
                        if not isFastForward
                           && (not options.AllowNonFastForward
                               || (isProtected refName && not options.AllowProtectedRefRewrite)) then
                            results.Add(refName, "ng non-fast-forward")
                        else
                            results.Add(refName, "ok")

                if results |> Seq.exists (fun (_, status) -> status = "ok") then
                    let updatedTips =
                        seq {
                            for index in 0 .. commands.Count - 1 do
                                let _, newSha, _ = commands.[index]
                                let _, status = results.[index]
                                if status = "ok" && newSha <> zeroId then yield newSha
                        }
                    match Fsck.verifyIntroduced repo updatedTips introducedObjects with
                    | Error error ->
                        unpackStatus <- error
                        for index in 0 .. results.Count - 1 do
                            let refName, status = results.[index]
                            if status = "ok" then results.[index] <- refName, "ng repository integrity error"
                    | Ok report when not report.IsValid ->
                        unpackStatus <- "introduced object validation failed"
                        for index in 0 .. results.Count - 1 do
                            let refName, status = results.[index]
                            if status = "ok" then results.[index] <- refName, "ng repository integrity error"
                    | Ok _ -> ()

                for index in 0 .. commands.Count - 1 do
                    let _, newSha, refName = commands.[index]
                    let _, status = results.[index]
                    if status = "ok" then
                        let applied =
                            if newSha = zeroId then References.deleteAtomic repo refName
                            else References.writeDirectAtomic repo refName newSha
                        match applied with
                        | Ok () -> ()
                        | Error error -> results.[index] <- refName, "ng " + error

                match References.readHead repo with
                | Ok (Symbolic target) when not (File.Exists(Path.Combine(repo.GitDir, target))) ->
                    match References.listBranches repo with
                    | Ok [| only |] -> References.updateHead repo (Symbolic("refs/heads/" + only)) |> ignore
                    | _ -> ()
                | _ -> ()

                PktLine.writeStr output (sprintf "unpack %s\n" unpackStatus)
                for refName, status in results do
                    if status = "ok" then PktLine.writeStr output (sprintf "ok %s\n" refName)
                    else PktLine.writeStr output (sprintf "ng %s %s\n" refName (status.Substring 3))
                PktLine.writeFlush output
                output.Flush()
            finally
                try
                    if Directory.Exists quarantinePath then Directory.Delete(quarantinePath, true)
                with _ -> ())

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
        w ms "agent=rbt-git/0.1\n"
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

    let private fetchV2To (repo: Repo) (args: string list) (output: Stream) : Result<unit, string> =
        let pick (prefix: string) =
            args |> List.choose (fun argument -> if argument.StartsWith prefix then Some(argument.Substring(prefix.Length).Trim()) else None)
        let wants =
            pick "want "
            |> List.choose (fun value -> if value.Length >= 40 then Some(value.Substring(0, 40)) else None)
            |> List.toArray
        let haves =
            pick "have "
            |> List.choose (fun value -> if value.Length >= 40 then Some(value.Substring(0, 40)) else None)
        if wants.Length = 0 then
            Error "no wants in fetch request"
        elif not (wantsAreAdvertised repo wants) then
            Error "want is not reachable from an advertised ref"
        else
            let doneSeen = List.contains "done" args
            let useDelta = List.contains "ofs-delta" args
            let depth =
                args
                |> List.tryPick (fun argument ->
                    if argument.StartsWith "deepen " then
                        match Int32.TryParse(argument.Substring(7).Trim()) with
                        | true, value -> Some value
                        | _ -> None
                    else None)
                |> Option.defaultValue 0
            let clientShallow = pick "shallow " |> Set.ofList

            let writeShallowInfo () =
                if depth > 0 then
                    let _, shallowSet = PackWriter.objectClosureShallow repo wants depth
                    PktLine.writeStr output "shallow-info\n"
                    for shallow in shallowSet do
                        if not (clientShallow.Contains shallow) then PktLine.writeStr output (sprintf "shallow %s\n" shallow)
                    PktLine.writeDelim output

            let writePackfile () =
                PktLine.writeStr output "packfile\n"
                let objects =
                    if depth > 0 then fst (PackWriter.objectClosureShallow repo wants depth)
                    else PackWriter.objectClosure repo wants (List.toArray haves)
                PackStream.writeTo repo objects useDelta true output

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
                    for hash in common do PktLine.writeStr output (sprintf "ACK %s\n" hash)
                    PktLine.writeStr output "ready\n"
                    PktLine.writeDelim output
                    writeShallowInfo ()
                    writePackfile ()
            Ok ()

    /// Dispatch a v2 command POST (ls-refs / fetch), streaming to `output`.
    let uploadPackV2To (repo: Repo) (reqBody: byte[]) (output: Stream) : Result<unit, string> =
        match parseV2Request reqBody with
        | "ls-refs", args ->
            let b = lsRefsV2 repo args
            output.Write(b, 0, b.Length)
            Ok()
        | "fetch", args -> fetchV2To repo args output
        | other, _ -> Error(sprintf "unsupported v2 command: %s" other)
