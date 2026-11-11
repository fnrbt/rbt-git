namespace Rbt.Git

open System
open System.Collections.Generic
open System.IO
open System.Net.Http
open System.Net.Http.Headers
open System.Text

module Remote =

    [<RequireQualifiedAccess>]
    type HttpAuth =
        | Basic of username: string * password: string
        | Bearer of token: string
        | Header of name: string * value: string
        | Signer of (HttpRequestMessage -> unit)

    type FetchReport = {
        RemoteRefs: Map<string, GitHash>
        UpdatedRefs: (string * GitHash) list
        PrunedRefs: string list
        ObjectCount: int
        PackBytes: int64
    }
    
    let listRemotes (repo: Repo) : Result<RemoteRepo[], string> =
        GitConfig.readConfig repo
        |> Result.map (fun config ->
            GitConfig.getSections config "remote"
            |> Array.choose (fun section -> section.Subsection)
            |> Array.distinct
            |> Array.choose (fun name ->
                match GitConfig.getValue config "remote" (Some name) "url" with
                | None -> None
                | Some url ->
                    let pushUrl =
                        GitConfig.getValue config "remote" (Some name) "pushurl"
                        |> Option.orElse (Some url)
                    Some { Name = name; FetchUrl = url; PushUrl = pushUrl }))
    
    let getRemote (repo: Repo) (name: string) : Result<RemoteRepo option, string> =
        match listRemotes repo with
        | Ok remotes ->
            Ok (remotes |> Array.tryFind (fun r -> r.Name = name))
        | Error msg -> Error msg
    
    let private zeroId = String('0', 40)

    let private validHash (s: string) =
        s.Length = 40 && s |> Seq.forall (fun c -> Char.IsDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))

    let private serviceUrl (remoteUrl: string) (suffix: string) =
        remoteUrl.TrimEnd('/') + suffix

    let private applyAuth (req: HttpRequestMessage) (auth: HttpAuth option) =
        match auth with
        | None -> ()
        | Some (HttpAuth.Basic(user, password)) ->
            let raw = Convert.ToBase64String(Encoding.UTF8.GetBytes(user + ":" + password))
            req.Headers.Authorization <- AuthenticationHeaderValue("Basic", raw)
        | Some (HttpAuth.Bearer token) ->
            req.Headers.Authorization <- AuthenticationHeaderValue("Bearer", token)
        | Some (HttpAuth.Header(name, value)) ->
            req.Headers.TryAddWithoutValidation(name, value) |> ignore
        | Some (HttpAuth.Signer sign) ->
            sign req

    let private httpBytes (client: HttpClient) (req: HttpRequestMessage) =
        task {
            let! resp = client.SendAsync(req)
            let! body = resp.Content.ReadAsByteArrayAsync()
            if not resp.IsSuccessStatusCode then
                let text =
                    try Encoding.UTF8.GetString(body)
                    with _ -> ""
                return Error(sprintf "HTTP %d %s%s" (int resp.StatusCode) resp.ReasonPhrase (if text = "" then "" else ": " + text))
            else
                return Ok body
        }

    let private parseAdvertisedRefs (body: byte[]) : Result<Map<string, GitHash> * string option, string> =
        try
            let refs = Dictionary<string, GitHash>()
            let mutable headSymref = None
            let mutable pos = 0
            let mutable finishedWithFlush = false
            while pos < body.Length do
                match PktLine.read body pos with
                | PktLine.Flush, np -> pos <- np; finishedWithFlush <- true
                | PktLine.Delim, _ -> raise (InvalidDataException "Unexpected advertisement delimiter")
                | PktLine.Data d, np ->
                    pos <- np
                    finishedWithFlush <- false
                    let line = Encoding.UTF8.GetString(d).TrimEnd('\n')
                    if not (line.StartsWith("# service=", StringComparison.Ordinal)) then
                        let nul = line.IndexOf('\000')
                        let refPart = if nul >= 0 then line.Substring(0, nul) else line
                        let caps = if nul >= 0 then line.Substring(nul + 1) else ""
                        if caps <> "" then
                            for cap in caps.Split(' ', StringSplitOptions.RemoveEmptyEntries) do
                                if cap.StartsWith("symref=HEAD:", StringComparison.Ordinal) then
                                    let target = cap.Substring("symref=HEAD:".Length)
                                    if not (References.isValidName target) then
                                        raise (InvalidDataException $"Invalid advertised HEAD target: {target}")
                                    headSymref <- Some target
                        let pieces = refPart.Split(' ', StringSplitOptions.RemoveEmptyEntries)
                        if pieces.Length <> 2 || not (validHash pieces.[0]) then
                            raise (InvalidDataException "Malformed advertised ref")
                        let sha = pieces.[0].ToLowerInvariant()
                        let name = pieces.[1]
                        if name <> "HEAD" && name <> "capabilities^{}" then
                            let baseName = if name.EndsWith("^{}", StringComparison.Ordinal) then name.Substring(0, name.Length - 3) else name
                            if not (References.isValidName baseName) then
                                raise (InvalidDataException $"Invalid advertised ref name: {name}")
                            if name = baseName then
                                if sha = zeroId then raise (InvalidDataException $"Invalid zero object ID for ref {name}")
                                if refs.ContainsKey name then raise (InvalidDataException $"Duplicate advertised ref: {name}")
                                refs.[name] <- sha
            if not finishedWithFlush then raise (InvalidDataException "Missing advertisement flush")
            Ok (refs |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq, headSymref)
        with ex ->
            Error("Failed to parse advertised refs: " + ex.Message)

    let private advertiseUploadPack (remoteUrl: string) (auth: HttpAuth option) =
        task {
            use client = new HttpClient()
            let url = serviceUrl remoteUrl "/info/refs?service=git-upload-pack"
            use req = new HttpRequestMessage(HttpMethod.Get, url)
            applyAuth req auth
            let! res = httpBytes client req
            match res with
            | Error e -> return Error e
            | Ok body -> return parseAdvertisedRefs body
        }

    let private listLocalRefs (repo: Repo) : Result<Map<string, GitHash>, string> =
        References.snapshot repo RefPolicy.Replication |> Result.map (fun snapshot -> snapshot.Refs)

    let private pkt (s: string) = PktLine.encodeStr s

    let private buildUploadPackRequest (wants: GitHash list) (haves: GitHash list) =
        use ms = new MemoryStream()
        let caps = "multi_ack_detailed side-band-64k ofs-delta agent=rbt-git-client/0.1"
        match wants with
        | [] -> ()
        | first :: rest ->
            let firstLine = sprintf "want %s %s\n" first caps
            let b = pkt firstLine
            ms.Write(b, 0, b.Length)
            for wnt in rest do
                let b = pkt (sprintf "want %s\n" wnt)
                ms.Write(b, 0, b.Length)
        PktLine.writeFlush ms
        for have in haves do
            let b = pkt (sprintf "have %s\n" have)
            ms.Write(b, 0, b.Length)
        let doneBytes = pkt "done\n"
        ms.Write(doneBytes, 0, doneBytes.Length)
        ms.ToArray()

    let private extractPack (body: byte[]) : Result<byte[] * string list, string> =
        try
            let pack = new MemoryStream()
            let progress = ResizeArray<string>()
            let mutable pos = 0
            let mutable rawPack = false
            let mutable remoteError: string option = None
            while pos < body.Length && not rawPack && Option.isNone remoteError do
                if pos + 4 <= body.Length && body.[pos] = byte 'P' && body.[pos + 1] = byte 'A' && body.[pos + 2] = byte 'C' && body.[pos + 3] = byte 'K' then
                    pack.Write(body, pos, body.Length - pos)
                    rawPack <- true
                else
                    match PktLine.read body pos with
                    | PktLine.Flush, np
                    | PktLine.Delim, np ->
                        pos <- np
                    | PktLine.Data d, np ->
                        pos <- np
                        if d.Length >= 4 &&
                           d.[0] = byte 'P' && d.[1] = byte 'A' && d.[2] = byte 'C' && d.[3] = byte 'K' then
                            pack.Write(d, 0, d.Length)
                        elif d.Length > 0 then
                            match d.[0] with
                            | 1uy -> pack.Write(d, 1, d.Length - 1)
                            | 2uy -> progress.Add(Encoding.UTF8.GetString(d, 1, d.Length - 1))
                            | 3uy -> remoteError <- Some(Encoding.UTF8.GetString(d, 1, d.Length - 1))
                            | _ ->
                                let text = Encoding.UTF8.GetString(d)
                                if not (text.StartsWith("NAK") || text.StartsWith("ACK")) then
                                    progress.Add text
            match remoteError with
            | Some err -> Error err
            | None -> Ok(pack.ToArray(), List.ofSeq progress)
        with ex ->
            Error("Failed to extract upload-pack response: " + ex.Message)


    type private RefspecMapping =
        | Prefix of sourcePrefix: string * destinationPrefix: string
        | Exact of source: string * destination: string

    let private parseRefspec (spec: string) =
        let body =
            if spec.StartsWith("+", StringComparison.Ordinal) then spec.Substring(1)
            else spec
        let parts = body.Split([| ':' |], 2)
        if parts.Length <> 2 then
            Error("Unsupported fetch refspec: " + spec)
        else
            let source = parts.[0]
            let destination = parts.[1]
            let sourceWildcards = source |> Seq.filter ((=) '*') |> Seq.length
            let destinationWildcards = destination |> Seq.filter ((=) '*') |> Seq.length
            let validRefName (name: string) = References.isValidName name
            if sourceWildcards = 0 && destinationWildcards = 0 && validRefName source && validRefName destination then
                Ok(Exact(source, destination))
            elif sourceWildcards = 1 && destinationWildcards = 1 &&
                 source.EndsWith("/*", StringComparison.Ordinal) &&
                 destination.EndsWith("/*", StringComparison.Ordinal) &&
                 validRefName (source.Substring(0, source.Length - 1) + "item") &&
                 validRefName (destination.Substring(0, destination.Length - 1) + "item") then
                Ok(Prefix(source.Substring(0, source.Length - 1), destination.Substring(0, destination.Length - 1)))
            else
                Error("Unsupported fetch refspec: " + spec)

    let private refsForRefspecs (remoteRefs: Map<string, GitHash>) (refspecs: string list) =
        let specs =
            if List.isEmpty refspecs then ["+refs/*:refs/*"]
            else refspecs
        let parsed = ResizeArray<RefspecMapping>()
        let mutable parseError = None
        for spec in specs do
            match parseRefspec spec with
            | Ok mapping -> parsed.Add mapping
            | Error e -> parseError <- Some e
        match parseError with
        | Some e -> Error e
        | None ->
            let mapped = Dictionary<string, GitHash>()
            let mutable conflict = None
            for KeyValue(sourceName, hash) in remoteRefs do
                for mapping in parsed do
                    let destination =
                        match mapping with
                        | Exact(source, destination) when sourceName = source ->
                            Some destination
                        | Prefix(sourcePrefix, destinationPrefix) when sourceName.StartsWith(sourcePrefix, StringComparison.Ordinal) ->
                            Some(destinationPrefix + sourceName.Substring(sourcePrefix.Length))
                        | _ -> None
                    match destination with
                    | None -> ()
                    | Some name ->
                        if not (References.isValidName name) then
                            conflict <- Some(sprintf "Invalid mapped ref: %s" name)
                        else
                            match mapped.TryGetValue name with
                            | true, existing when existing <> hash ->
                                conflict <- Some(sprintf "Multiple remote refs map to %s" name)
                            | _ -> mapped.[name] <- hash
            let pruneAppliesTo name =
                parsed
                |> Seq.exists (function
                    | Exact(_, destination) -> name = destination
                    | Prefix(_, destinationPrefix) -> name.StartsWith(destinationPrefix, StringComparison.Ordinal))
            match conflict with
            | Some e -> Error e
            | None ->
                Ok(mapped |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq, pruneAppliesTo)


    /// Fetch a mirror through a caller-supplied authenticated transport.
    /// The callbacks exchange ordinary smart-HTTP advertisement and
    /// upload-pack payload bytes without imposing HTTP as the wire transport.
    let fetchMirrorWithTransport
        (repo: Repo)
        (advertise: unit -> Result<byte[], string>)
        (uploadPack: byte[] -> Result<byte[], string>)
        (refspecs: string list)
        (prune: bool) : Result<FetchReport, string> =
        let invalidBranchTarget (readRaw: GitHash -> Result<string * byte[], string>) (refs: Map<string, GitHash>) =
            refs
            |> Map.exists (fun name hash ->
                (name.StartsWith("refs/heads/", StringComparison.Ordinal) ||
                 name.StartsWith("refs/remotes/", StringComparison.Ordinal)) &&
                (match readRaw hash with
                 | Ok ("commit", _) -> false
                 | _ -> true))

        let finish validated remoteRefs headSymref pruneAppliesTo localRefs objectCount packByteCount =
            let connectivity =
                if validated then Ok ()
                else
                    match Fsck.verifyReachable repo { Head = Symbolic ""; Refs = remoteRefs; Digest = "" } with
                    | Error error -> Error error
                    | Ok report when not report.IsValid -> Error "Fetched refs have incomplete or invalid object connectivity"
                    | Ok _ -> Ok ()
            match connectivity with
            | Error error -> Error error
            | Ok () when invalidBranchTarget (ReadObjects.readRawObject repo) remoteRefs ->
                Error "Fetched branch does not target a commit"
            | Ok () ->
                let updated = ResizeArray<string * GitHash>()
                let pruned = ResizeArray<string>()
                let mutable failure = None
                for KeyValue(name, hash) in remoteRefs do
                    if Map.tryFind name localRefs <> Some hash && Option.isNone failure then
                        match References.compareAndSwap repo name (Map.tryFind name localRefs) (Some hash) with
                        | Ok () -> updated.Add(name, hash)
                        | Error error -> failure <- Some error
                if prune && Option.isNone failure then
                    for KeyValue(name, hash) in localRefs do
                        if pruneAppliesTo name && not (Map.containsKey name remoteRefs) && Option.isNone failure then
                            match References.compareAndSwap repo name (Some hash) None with
                            | Ok () -> pruned.Add name
                            | Error error -> failure <- Some error
                match failure with
                | Some error -> Error error
                | None ->
                    match headSymref with
                    | Some target when Map.containsKey target remoteRefs ->
                        match References.updateHead repo (Symbolic target) with
                        | Error error -> failure <- Some error
                        | Ok () -> ()
                    | _ -> ()
                    match failure with
                    | Some error -> Error error
                    | None ->
                        Ok {
                            RemoteRefs = remoteRefs
                            UpdatedRefs = List.ofSeq updated
                            PrunedRefs = List.ofSeq pruned
                            ObjectCount = objectCount
                            PackBytes = packByteCount
                        }

        match advertise () with
        | Error error -> Error error
        | Ok advertisement ->
            match parseAdvertisedRefs advertisement with
            | Error error -> Error error
            | Ok (advertisedRefs, headSymref) ->
                match refsForRefspecs advertisedRefs refspecs, listLocalRefs repo with
                | Error error, _ | _, Error error -> Error error
                | Ok (remoteRefs, pruneAppliesTo), Ok localRefs ->
                    let wants =
                        remoteRefs
                        |> Map.toSeq
                        |> Seq.map snd
                        |> Seq.distinct
                        |> Seq.filter (fun hash -> not (ReadObjects.objectExists repo hash))
                        |> Seq.toList
                    let haves = localRefs |> Map.toSeq |> Seq.map snd |> Seq.distinct |> Seq.toList
                    if List.isEmpty wants then
                        finish false remoteRefs headSymref pruneAppliesTo localRefs 0 0L
                    else
                        match uploadPack (buildUploadPackRequest wants haves) with
                        | Error error -> Error error
                        | Ok response ->
                            match extractPack response with
                            | Error error -> Error error
                            | Ok (packBytes, _) when packBytes.Length = 0 ->
                                Error "Remote did not provide the requested pack"
                            | Ok (packBytes, _) ->
                                let quarantinePath = repo.GitDir + ".fetch-quarantine-" + Guid.NewGuid().ToString("N")
                                try
                                    match Repository.initBare quarantinePath with
                                    | Error error -> Error error
                                    | Ok quarantine ->
                                        use input = new MemoryStream(packBytes)
                                        use counting = new PackData.CountingStream(input)
                                        match PackWriter.unpackPackStreamInto repo quarantine counting with
                                        | Error error -> Error error
                                        | Ok written ->
                                            match Fsck.verifyReachableOverlay repo quarantine remoteRefs written with
                                            | Error error -> Error error
                                            | Ok report when not report.IsValid ->
                                                Error "Fetched refs have incomplete or invalid object connectivity"
                                            | Ok _ ->
                                                let incoming = HashSet<GitHash>(written, StringComparer.Ordinal)
                                                let readRaw hash =
                                                    ReadObjects.readRawObject (if incoming.Contains hash then quarantine else repo) hash
                                                if invalidBranchTarget readRaw remoteRefs then
                                                    Error "Fetched branch does not target a commit"
                                                else
                                                    match PackWriter.promoteLooseObjects quarantine repo written with
                                                    | Error error -> Error error
                                                    | Ok () ->
                                                        finish true remoteRefs headSymref pruneAppliesTo localRefs written.Length (int64 packBytes.Length)
                                finally
                                    try
                                        if Directory.Exists quarantinePath then Directory.Delete(quarantinePath, true)
                                    with _ -> ()

    let fetchMirrorWithAuth (repo: Repo) (remoteUrl: string) (auth: HttpAuth option) (refspecs: string list) (prune: bool) : Result<FetchReport, string> =
        if not (remoteUrl.StartsWith("http://", StringComparison.OrdinalIgnoreCase) || remoteUrl.StartsWith("https://", StringComparison.OrdinalIgnoreCase)) then
            Error "Only HTTP(S) mirror fetch is implemented"
        else
            let advertise () =
                use client = new HttpClient()
                use request = new HttpRequestMessage(HttpMethod.Get, serviceUrl remoteUrl "/info/refs?service=git-upload-pack")
                applyAuth request auth
                httpBytes client request |> Async.AwaitTask |> Async.RunSynchronously
            let uploadPack requestBody =
                use client = new HttpClient()
                use request = new HttpRequestMessage(HttpMethod.Post, serviceUrl remoteUrl "/git-upload-pack")
                applyAuth request auth
                request.Content <- new ByteArrayContent(requestBody)
                request.Content.Headers.ContentType <- MediaTypeHeaderValue("application/x-git-upload-pack-request")
                request.Headers.Accept.Add(MediaTypeWithQualityHeaderValue("application/x-git-upload-pack-result"))
                httpBytes client request |> Async.AwaitTask |> Async.RunSynchronously
            fetchMirrorWithTransport repo advertise uploadPack refspecs prune

    let fetchMirror (repo: Repo) (remoteUrl: string) (refspecs: string list) (prune: bool) : Result<FetchReport, string> =
        fetchMirrorWithAuth repo remoteUrl None refspecs prune

    let lsRemoteHttp (remoteUrl: string) (auth: HttpAuth option) : Result<Map<string, GitHash>, string> =
        advertiseUploadPack remoteUrl auth
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> Result.map fst

    let private gitProtocolFetch (_remoteUrl: string) (_refs: string[]) : Result<Map<string, GitHash>, string> =
        Error "Git protocol fetch is unsupported"

    let private sshFetch (_remoteUrl: string) (_refs: string[]) : Result<Map<string, GitHash>, string> =
        Error "SSH fetch is unsupported"
    
    let fetch (repo: Repo) (remote: string) : Result<unit, string> =
        match getRemote repo remote with
        | Ok (Some remoteObj) ->
            let protocol = 
                if remoteObj.FetchUrl.StartsWith "http://" || remoteObj.FetchUrl.StartsWith "https://" then
                    "http"
                elif remoteObj.FetchUrl.StartsWith "git://" then
                    "git"
                elif remoteObj.FetchUrl.Contains "@" && remoteObj.FetchUrl.Contains ":" then
                    "ssh"
                else
                    "file"
            
            match protocol with
            | "http" ->
                match fetchMirror repo remoteObj.FetchUrl ["+refs/heads/*:refs/remotes/" + remoteObj.Name + "/*"; "+refs/tags/*:refs/tags/*"] false with
                | Ok _ -> Ok ()
                | Error msg -> Error msg
            | "git" ->
                match gitProtocolFetch remoteObj.FetchUrl [||] with
                | Ok _ -> Ok ()
                | Error msg -> Error msg
            | "ssh" ->
                match sshFetch remoteObj.FetchUrl [||] with
                | Ok _ -> Ok ()
                | Error msg -> Error msg
            | "file" ->
                Error "File transport fetch is unsupported; no refs or objects were changed"
            | _ ->
                Error $"Unsupported protocol: {protocol}"
        | Ok None -> Error $"Remote not found: {remote}"
        | Error msg -> Error msg
    
    let push (repo: Repo) (remote: string) (branch: string) : Result<unit, string> =
        match getRemote repo remote with
        | Ok (Some remoteObj) ->
            Error "Push not yet implemented"
        | Ok None -> Error $"Remote not found: {remote}"
        | Error msg -> Error msg
    
    let lsRemote (repo: Repo) (remote: string) : Result<Map<string, GitHash>, string> =
        match getRemote repo remote with
        | Ok (Some remoteObj) ->
            if remoteObj.FetchUrl.StartsWith "http://" || remoteObj.FetchUrl.StartsWith "https://" then
                lsRemoteHttp remoteObj.FetchUrl None
            else
                Error "ls-remote is only implemented for HTTP(S) remotes"
        | Ok None -> Error $"Remote not found: {remote}"
        | Error msg -> Error msg
    
    let private validRemoteName (name: string) =
        not (String.IsNullOrEmpty name) &&
        References.isValidName ("refs/remotes/" + name + "/test")

    let addRemote (repo: Repo) (name: string) (url: string) : Result<unit, string> =
        if not (validRemoteName name) then Error $"Invalid remote name: {name}"
        elif String.IsNullOrWhiteSpace url then Error "Remote URL must not be empty"
        else
            GitConfig.updateConfig repo (fun config ->
                if GitConfig.hasSection config "remote" (Some name) then
                    Error $"Remote {name} already exists"
                else Ok(GitConfig.setValue config "remote" (Some name) "url" url))

    let removeRemote (repo: Repo) (name: string) : Result<unit, string> =
        if not (validRemoteName name) then Error $"Invalid remote name: {name}"
        else
            GitConfig.updateConfig repo (fun config ->
                if GitConfig.hasSection config "remote" (Some name) then
                    Ok(GitConfig.removeSection config "remote" (Some name))
                else Error $"Remote {name} not found")
