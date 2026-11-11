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

    let private snapshot (repo: Repo) (policy: RefPolicy) =
        References.snapshot repo policy
        |> Result.defaultWith (fun error -> raise (InvalidDataException error))

    let rec resolveFull (repo: Repo) (name: string) (depth: int) : GitHash option =
        if depth > 10 || (name <> "HEAD" && not (References.isValidName name)) then None
        else
            match References.snapshot repo RefPolicy.Replication with
            | Error _ -> None
            | Ok refs ->
                if name = "HEAD" then
                    match refs.Head with
                    | Direct hash -> Some hash
                    | Symbolic target -> Map.tryFind target refs.Refs
                else Map.tryFind name refs.Refs

    let private gatherRefs (repo: Repo) : (GitHash * string) list =
        (snapshot repo RefPolicy.Public).Refs
        |> Map.toList
        |> List.map (fun (name, hash) -> hash, name)

    let private wantsAreAdvertised (repo: Repo) (wants: seq<GitHash>) =
        let advertised = System.Collections.Generic.HashSet<GitHash>(StringComparer.Ordinal)
        for hash, _ in gatherRefs repo do
            advertised.Add(hash) |> ignore
            match ReadObjects.readObject repo hash with
            | Ok (GitObject.Tag tag) -> advertised.Add(tag.Object) |> ignore
            | _ -> ()
        wants |> Seq.forall (fun hash -> Hashing.isValidHash hash && advertised.Contains hash)

    /// Ref advertisement for Git protocol v0/v1 without an HTTP service header.
    /// This is what direct transports such as SSH write immediately after exec.
    let private advertiseRefsRawWith (service: string) (refs: (GitHash * string) list) (headTarget: string option) : byte[] =
        use ms = new MemoryStream()
        let baseCaps =
            match service with
            | "git-upload-pack" -> "shallow deepen-relative ofs-delta side-band-64k agent=rbt-git/0.1"
            | _ -> "report-status delete-refs ofs-delta agent=rbt-git/0.1"
        let caps =
            match service, headTarget with
            | "git-upload-pack", Some target -> sprintf "%s symref=HEAD:%s" baseCaps target
            | _ -> baseCaps
        let entries =
            match service, headTarget with
            | "git-upload-pack", Some target ->
                match refs |> List.tryFind (fun (_, name) -> name = target) with
                | Some (hash, _) -> (hash, "HEAD") :: refs
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
        let refs = snapshot repo RefPolicy.Public
        let headTarget =
            match refs.Head with
            | Symbolic target when Map.containsKey target refs.Refs -> Some target
            | _ -> None
        advertiseRefsRawWith service (refs.Refs |> Map.toList |> List.map (fun (name, hash) -> hash, name)) headTarget

    /// Body of GET /info/refs?service=<service>.
    let advertiseRefs (repo: Repo) (service: string) : byte[] =
        use ms = new MemoryStream()
        w ms (sprintf "# service=%s\n" service)
        PktLine.writeFlush ms
        let body = advertiseRefsRaw repo service
        ms.Write(body, 0, body.Length)
        ms.ToArray()

    let private effectiveDepth (repo: Repo) (wants: GitHash[]) (clientShallow: seq<GitHash>) (relative: bool) (requested: int) =
        if not relative || requested <= 0 then requested
        else
            let shallow = System.Collections.Generic.HashSet<GitHash>(clientShallow, StringComparer.Ordinal)
            let seen = System.Collections.Generic.HashSet<GitHash>(StringComparer.Ordinal)
            let pending = System.Collections.Generic.Queue<GitHash * int>()
            for hash in wants do pending.Enqueue(hash, 1)
            let mutable boundary = None
            while pending.Count > 0 && Option.isNone boundary do
                let hash, distance = pending.Dequeue()
                if seen.Add hash then
                    if shallow.Contains hash then boundary <- Some distance
                    else
                        match ReadObjects.readObject repo hash with
                        | Ok (GitObject.Commit commit) ->
                            for parent in commit.Parents do pending.Enqueue(parent, distance + 1)
                        | Ok (GitObject.Tag tag) -> pending.Enqueue(tag.Object, distance)
                        | _ -> ()
            match boundary with
            | Some distance when requested <= Int32.MaxValue - distance -> requested + distance
            | _ -> requested
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
        let clientShallow = System.Collections.Generic.HashSet<GitHash>(StringComparer.Ordinal)
        let mutable p = 0
        let mutable doneSeen = false
        let mutable sawFlush = false
        let parseError =
            try
                while p < reqBody.Length && not doneSeen do
                    match PktLine.read reqBody p with
                    | PktLine.Flush, np -> p <- np; sawFlush <- true
                    | PktLine.Delim, _ -> raise (InvalidDataException "Unexpected upload-pack delimiter")
                    | PktLine.Data d, np ->
                        p <- np
                        let s = Encoding.UTF8.GetString d
                        if s.StartsWith "want " && s.Length >= 45 then
                            wants.Add(s.Substring(5, 40))
                            if caps = "" && s.Length > 45 then caps <- s.Substring(45)
                        elif s.StartsWith "have " && s.Length >= 45 && Hashing.isValidHash(s.Substring(5, 40)) then haves.Add(s.Substring(5, 40))
                        elif s.StartsWith "shallow " && s.Length >= 48 && Hashing.isValidHash(s.Substring(8, 40)) then clientShallow.Add(s.Substring(8, 40)) |> ignore
                        elif s.StartsWith "deepen " then
                            match Int32.TryParse(s.Substring(7).Trim()) with
                            | true, n when n > 0 -> depth <- n
                            | _ -> raise (InvalidDataException "Invalid deepen depth")
                        elif s = "done\n" then doneSeen <- true
                        else raise (InvalidDataException "Invalid upload-pack request line")
                if not sawFlush then Some "Missing upload-pack flush"
                elif doneSeen && p <> reqBody.Length then Some "Unexpected data after done"
                else None
            with :? InvalidDataException as error -> Some error.Message
        if Option.isSome parseError then Error(parseError.Value)
        elif wants.Count = 0 then Error "no wants in upload-pack request"
        elif not (authorize wants) then Error "want is not reachable from an advertised ref"
        else
            let useDelta = caps.Contains "ofs-delta"
            let useSideband = caps.Contains "side-band-64k"
            let depth = effectiveDepth repo (wants.ToArray()) clientShallow (caps.Contains "deepen-relative") depth
            if depth > 0 then
                // Shallow negotiation is two POSTs. POST 1 (wants+deepen+flush, no
                // "done") gets only the shallow boundary; POST 2 (adds "done") gets
                // the shallow boundary + NAK + the depth-limited pack.
                let objs, shallowSet = PackWriter.objectClosureShallow repo (wants.ToArray()) depth
                let boundary = Set.ofArray shallowSet
                for sh in clientShallow do
                    if Array.contains sh objs && not (boundary.Contains sh) then PktLine.writeStr output (sprintf "unshallow %s\n" sh)
                for sh in shallowSet do
                    if not (clientShallow.Contains sh) then PktLine.writeStr output (sprintf "shallow %s\n" sh)
                PktLine.writeFlush output
                if doneSeen then
                    PktLine.writeStr output "NAK\n"
                    PackStream.writeTo repo objs useDelta useSideband output
                elif haves.Count > 0 then
                    PktLine.writeStr output "NAK\n"
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
                let commonHaves = haves |> Seq.filter (ReadObjects.objectExists repo) |> Seq.toArray
                let objs = PackWriter.objectClosure repo (wants.ToArray()) commonHaves
                PktLine.writeStr output "NAK\n"
                PackStream.writeTo repo objs useDelta useSideband output
                Ok()

    let uploadPackTo (repo: Repo) (reqBody: byte[]) (output: Stream) : Result<unit, string> =
        try uploadPackToWithAuthorization (wantsAreAdvertised repo) repo reqBody output
        with ex -> Error $"Upload-pack failed: {ex.Message}"

    let advertiseRefsForPolicy (repo: Repo) (service: string) (policy: RefPolicy) : Result<byte[], string> =
        References.snapshot repo policy
        |> Result.map (fun refs ->
            use ms = new MemoryStream()
            w ms (sprintf "# service=%s\n" service)
            PktLine.writeFlush ms
            let headTarget =
                match refs.Head with
                | Symbolic target when Map.containsKey target refs.Refs -> Some target
                | _ -> None
            let body = advertiseRefsRawWith service (refs.Refs |> Map.toList |> List.map (fun (name, hash) -> hash, name)) headTarget
            ms.Write(body, 0, body.Length)
            ms.ToArray())

    let uploadPackToForPolicy
        (repo: Repo)
        (policy: RefPolicy)
        (reqBody: byte[])
        (output: Stream) : Result<unit, string> =
        try
            match References.snapshot repo policy with
            | Error error -> Error error
            | Ok snapshot ->
                let allowed = System.Collections.Generic.HashSet<GitHash>(snapshot.Refs.Values, StringComparer.Ordinal)
                let authorize wants =
                    wants
                    |> Seq.forall (fun hash -> Hashing.isValidHash hash && allowed.Contains hash)
                uploadPackToWithAuthorization authorize repo reqBody output
        with ex -> Error $"Upload-pack failed: {ex.Message}"

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
    let private uploadPackStreamCore (repo: Repo) (input: Stream) (output: Stream) : Result<unit, string> =
        let wants = ResizeArray<GitHash>()
        let haves = ResizeArray<GitHash>()
        let mutable caps = ""
        let mutable depth = 0
        let mutable doneSeen = false
        let mutable eof = false
        let mutable sentShallow = false
        let mutable invalidWant = false
        let mutable malformed = false
        let mutable negotiatingHaves = false
        let clientShallow = System.Collections.Generic.HashSet<GitHash>(StringComparer.Ordinal)

        let resolvedDepth () = effectiveDepth repo (wants.ToArray()) clientShallow (caps.Contains "deepen-relative") depth
        let shallowInfo () =
            if depth > 0 && not sentShallow then
                let objs, shallowSet = PackWriter.objectClosureShallow repo (wants.ToArray()) (resolvedDepth ())
                let boundary = Set.ofArray shallowSet
                for sh in clientShallow do
                    if Array.contains sh objs && not (boundary.Contains sh) then PktLine.writeStr output (sprintf "unshallow %s\n" sh)
                for sh in shallowSet do
                    if not (clientShallow.Contains sh) then PktLine.writeStr output (sprintf "shallow %s\n" sh)
                PktLine.writeFlush output
                output.Flush()
                sentShallow <- true
        try
            while not doneSeen && not eof do
                match PktLine.readFrom input with
                | Some (PktLine.Data d) ->
                    let s = Encoding.UTF8.GetString d
                    if s.StartsWith "want " && s.Length >= 45 then
                        wants.Add(s.Substring(5, 40))
                        if caps = "" && s.Length > 45 then caps <- s.Substring(45)
                    elif s.StartsWith "have " && s.Length >= 45 && Hashing.isValidHash(s.Substring(5, 40)) then
                        haves.Add(s.Substring(5, 40))
                        negotiatingHaves <- true
                    elif s.StartsWith "shallow " && s.Length >= 48 && Hashing.isValidHash(s.Substring(8, 40)) then clientShallow.Add(s.Substring(8, 40)) |> ignore
                    elif s.StartsWith "deepen " then
                        match Int32.TryParse(s.Substring(7).Trim()) with
                        | true, n when n > 0 -> depth <- n
                        | _ -> malformed <- true
                    elif s = "done\n" then doneSeen <- true
                    else malformed <- true
                | Some PktLine.Flush ->
                    if wants.Count > 0 then
                        if wantsAreAdvertised repo wants then
                            shallowInfo()
                            if negotiatingHaves then
                                PktLine.writeStr output "NAK\n"
                                output.Flush()
                                negotiatingHaves <- false
                        else
                            invalidWant <- true
                            doneSeen <- true
                | Some PktLine.Delim -> malformed <- true
                | None -> eof <- true
        with :? InvalidDataException -> malformed <- true

        if malformed then Error "Malformed upload-pack request"
        elif wants.Count = 0 then Error "no wants in upload-pack request"
        elif invalidWant || not (wantsAreAdvertised repo wants) then
            Error "want is not reachable from an advertised ref"
        elif not doneSeen then Error "upload-pack request ended before done"
        else
            shallowInfo()
            PktLine.writeStr output "NAK\n"
            let useDelta = caps.Contains "ofs-delta"
            let useSideband = caps.Contains "side-band-64k"
            let objs =
                if depth > 0 then fst (PackWriter.objectClosureShallow repo (wants.ToArray()) (resolvedDepth ()))
                else
                    let commonHaves = haves |> Seq.filter (ReadObjects.objectExists repo) |> Seq.toArray
                    PackWriter.objectClosure repo (wants.ToArray()) commonHaves
            PackStream.writeTo repo objs useDelta useSideband output
            Ok()

    let uploadPackStream (repo: Repo) (input: Stream) (output: Stream) : Result<unit, string> =
        try uploadPackStreamCore repo input output
        with ex -> Error $"Upload-pack failed: {ex.Message}"

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
        let mutable malformed = false
        let mutable flushed = false
        try
            while not flushed do
                match PktLine.readFrom buf with
                | Some (PktLine.Data data) ->
                    let raw = Encoding.UTF8.GetString data
                    let line = (let index = raw.IndexOf('\000') in if index >= 0 then raw.Substring(0, index) else raw).TrimEnd('\n')
                    match line.Split(' ', StringSplitOptions.RemoveEmptyEntries) with
                    | [| oldSha; newSha; refName |] ->
                        if not (Hashing.isValidHash oldSha && Hashing.isValidHash newSha && References.isValidName refName) then malformed <- true
                        else commands.Add(oldSha.ToLowerInvariant(), newSha.ToLowerInvariant(), refName)
                    | _ -> malformed <- true
                | Some PktLine.Flush -> flushed <- true
                | Some PktLine.Delim | None -> malformed <- true; flushed <- true
        with :? InvalidDataException -> malformed <- true

        lock (lockFor repo) (fun () ->
            let results = ResizeArray<string * string>()
            let mutable unpackStatus = if malformed || commands.Count = 0 then "invalid ref command" else "ok"
            let refsBefore = References.snapshot repo RefPolicy.Replication
            match refsBefore with
            | Error error when unpackStatus = "ok" -> unpackStatus <- error
            | _ -> ()
            let quarantinePath = repo.GitDir + ".quarantine-" + Guid.NewGuid().ToString("N")
            try
                if unpackStatus = "ok" then
                    match Repository.initBare quarantinePath with
                    | Error error -> unpackStatus <- error
                    | Ok quarantine ->
                        let mutable introduced: GitHash[] = [||]
                        if commands |> Seq.exists (fun (_, newSha, _) -> newSha <> zeroId) then
                            use counting = new PackData.CountingStream(buf)
                            match PackWriter.unpackPackStreamInto repo quarantine counting with
                            | Error error -> unpackStatus <- error
                            | Ok written -> introduced <- written

                        let introducedSet = System.Collections.Generic.HashSet<GitHash>(introduced, StringComparer.Ordinal)
                        let readObject hash =
                            if introducedSet.Contains hash then ReadObjects.readObject quarantine hash
                            else ReadObjects.readObject repo hash

                        let isAncestor oldSha newSha =
                            let seen = System.Collections.Generic.HashSet<string>(StringComparer.Ordinal)
                            let pending = System.Collections.Generic.Stack<string>()
                            pending.Push newSha
                            let mutable found = false
                            while pending.Count > 0 && not found do
                                let hash = pending.Pop()
                                if hash = oldSha then found <- true
                                elif seen.Add hash then
                                    match readObject hash with
                                    | Ok (GitObject.Commit commit) ->
                                        for parent in commit.Parents do pending.Push parent
                                    | _ -> ()
                            found

                        for oldSha, newSha, refName in commands do
                            let current =
                                match refsBefore with
                                | Ok refs -> Map.tryFind refName refs.Refs |> Option.defaultValue zeroId
                                | Error _ -> zeroId
                            let status =
                                if unpackStatus <> "ok" then "ng unpacker error"
                                elif current <> oldSha then "ng fetch first"
                                elif newSha = zeroId then
                                    if List.contains refName options.ProtectedRefs then "ng protected ref" else "ok"
                                else
                                    match readObject newSha with
                                    | Error _ -> "ng missing object"
                                    | Ok objectValue when
                                        (refName.StartsWith("refs/heads/", StringComparison.Ordinal) ||
                                         refName.StartsWith("refs/remotes/", StringComparison.Ordinal)) &&
                                        (match objectValue with GitObject.Commit _ -> false | _ -> true) ->
                                        "ng branch must target commit"
                                    | Ok _ ->
                                        let isFastForward = oldSha = zeroId || isAncestor oldSha newSha
                                        if not isFastForward &&
                                           (not options.AllowNonFastForward ||
                                            (List.contains refName options.ProtectedRefs && not options.AllowProtectedRefRewrite)) then
                                            "ng non-fast-forward"
                                        else "ok"
                            results.Add(refName, status)

                        let acceptedTips =
                            seq {
                                for index in 0 .. commands.Count - 1 do
                                    let _, hash, _ = commands.[index]
                                    if hash <> zeroId && snd results.[index] = "ok" then yield hash
                            }
                            |> Seq.toArray
                        if results |> Seq.exists (fun (_, status) -> status = "ok") then
                            match Fsck.verifyIntroducedOverlay repo quarantine acceptedTips introduced with
                            | Error error -> unpackStatus <- error
                            | Ok report when not report.IsValid -> unpackStatus <- "introduced object validation failed"
                            | Ok _ ->
                                if acceptedTips.Length > 0 then
                                    match PackWriter.promoteLooseObjects quarantine repo introduced with
                                    | Error error -> unpackStatus <- error
                                    | Ok () -> ()
                            if unpackStatus <> "ok" then
                                for index in 0 .. results.Count - 1 do
                                    let name, status = results.[index]
                                    if status = "ok" then results.[index] <- name, "ng repository integrity error"

                        if unpackStatus = "ok" then
                            for index in 0 .. commands.Count - 1 do
                                let oldSha, newSha, refName = commands.[index]
                                let _, status = results.[index]
                                if status = "ok" then
                                    let oldValue = if oldSha = zeroId then None else Some oldSha
                                    let newValue = if newSha = zeroId then None else Some newSha
                                    match References.compareAndSwap repo refName oldValue newValue with
                                    | Ok () -> ()
                                    | Error error -> results.[index] <- refName, "ng " + error

                        if unpackStatus = "ok" then
                            match References.readHead repo, References.snapshot repo RefPolicy.Public with
                            | Ok (Symbolic target), Ok refs when not (Map.containsKey target refs.Refs) ->
                                let branches = refs.Refs |> Map.toList |> List.filter (fun (name, _) -> name.StartsWith("refs/heads/", StringComparison.Ordinal))
                                match branches with
                                | [ name, _ ] -> References.updateHead repo (Symbolic name) |> ignore
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
        if not stop || p <> reqBody.Length || not afterDelim then
            raise (InvalidDataException "Malformed v2 request")
        command, List.ofSeq args

    let private lsRefsV2 (repo: Repo) (args: string list) : byte[] =
        let wantSymrefs = List.contains "symrefs" args
        let wantPeel = List.contains "peel" args
        let wantUnborn = List.contains "unborn" args
        let prefixes = args |> List.choose (fun a -> if a.StartsWith "ref-prefix " then Some(a.Substring 11) else None)
        let matches (name: string) = List.isEmpty prefixes || prefixes |> List.exists (fun pre -> name.StartsWith pre)
        use ms = new MemoryStream()
        let refs = snapshot repo RefPolicy.Public
        (match refs.Head with
         | Symbolic target when matches "HEAD" ->
             match Map.tryFind target refs.Refs with
             | Some sha ->
                 let line = sprintf "%s HEAD" sha
                 w ms ((if wantSymrefs then line + sprintf " symref-target:%s" target else line) + "\n")
             | None when wantUnborn && target.StartsWith("refs/heads/", StringComparison.Ordinal) &&
                         not (Map.containsKey target (snapshot repo RefPolicy.Replication).Refs) ->
                 let line = "unborn HEAD"
                 w ms ((if wantSymrefs then line + sprintf " symref-target:%s" target else line) + "\n")
             | None -> ()
         | _ -> ())
        for (name, sha) in Map.toList refs.Refs do
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
            |> List.toArray
        let haves = pick "have "
        let malformedIds =
            args
            |> List.exists (fun argument ->
                [ "want "; "have "; "shallow " ]
                |> List.exists (fun prefix ->
                    if argument.StartsWith(prefix, StringComparison.Ordinal) then
                        not (Hashing.isValidHash(argument.Substring(prefix.Length)))
                    else false))
        let malformedDepth =
            args |> List.exists (fun argument ->
                if argument.StartsWith("deepen ", StringComparison.Ordinal) then
                    match Int32.TryParse(argument.Substring(7).Trim()) with
                    | true, depth when depth > 0 -> false
                    | _ -> true
                else false)
        if malformedIds || malformedDepth then Error "Malformed fetch request"
        elif wants.Length = 0 then
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

            let depth = effectiveDepth repo wants clientShallow (List.contains "deepen-relative" args) depth
            let writeShallowInfo () =
                if depth > 0 then
                    let objs, shallowSet = PackWriter.objectClosureShallow repo wants depth
                    let boundary = Set.ofArray shallowSet
                    PktLine.writeStr output "shallow-info\n"
                    for shallow in clientShallow do
                        if Array.contains shallow objs && not (boundary.Contains shallow) then PktLine.writeStr output (sprintf "unshallow %s\n" shallow)
                    for shallow in shallowSet do
                        if not (clientShallow.Contains shallow) then PktLine.writeStr output (sprintf "shallow %s\n" shallow)
                    PktLine.writeDelim output

            let writePackfile () =
                PktLine.writeStr output "packfile\n"
                let objects =
                    if depth > 0 then fst (PackWriter.objectClosureShallow repo wants depth)
                    else PackWriter.objectClosure repo wants (haves |> List.filter (ReadObjects.objectExists repo) |> List.toArray)
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
        try
            match parseV2Request reqBody with
            | "ls-refs", args ->
                let b = lsRefsV2 repo args
                output.Write(b, 0, b.Length)
                Ok()
            | "fetch", args -> fetchV2To repo args output
            | other, _ -> Error(sprintf "unsupported v2 command: %s" other)
        with
        | :? InvalidDataException as error -> Error error.Message
        | ex -> Error $"Upload-pack failed: {ex.Message}"
