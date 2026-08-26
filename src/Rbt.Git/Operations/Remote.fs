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
    
    let private parseRemoteConfig (content: string) : RemoteRepo[] =
        let lines = content.Split('\n')
        let mutable remotes = ResizeArray<RemoteRepo>()
        let mutable currentRemote: string option = None
        let mutable fetchUrl: string option = None
        let mutable pushUrl: string option = None
        
        let processLine (line: string) : unit =
            let trimmed = line.Trim()
            if trimmed.StartsWith "[remote \"" then
                match currentRemote, fetchUrl, pushUrl with
                | Some name, Some url, _ ->
                    let remote = { Name = name; FetchUrl = url; PushUrl = pushUrl } : RemoteRepo
                    remotes.Add remote |> ignore
                | _ -> ()
                
                let endQuote = trimmed.IndexOf('"', 9)
                if endQuote > 0 then
                    currentRemote <- Some trimmed.[9..endQuote - 1]
                    fetchUrl <- None
                    pushUrl <- None
                else
                    currentRemote <- None
                    fetchUrl <- None
                    pushUrl <- None
            elif trimmed.StartsWith "]" then
                match currentRemote, fetchUrl, pushUrl with
                | Some name, Some url, _ ->
                    let remote = { Name = name; FetchUrl = url; PushUrl = pushUrl } : RemoteRepo
                    remotes.Add remote |> ignore
                | _ -> ()
                currentRemote <- None
                fetchUrl <- None
                pushUrl <- None
            elif trimmed.StartsWith "url" then
                let parts = trimmed.Split('=')
                if parts.Length >= 2 then
                    fetchUrl <- Some (parts.[1].Trim())
                    if Option.isNone pushUrl then
                        pushUrl <- fetchUrl
            elif trimmed.StartsWith "pushurl" then
                let parts = trimmed.Split('=')
                if parts.Length >= 2 then
                    pushUrl <- Some (parts.[1].Trim())
        
        for line in lines do
            processLine line
        
        match currentRemote, fetchUrl, pushUrl with
        | Some name, Some url, _ ->
            let remote = { Name = name; FetchUrl = url; PushUrl = pushUrl } : RemoteRepo
            remotes.Add remote |> ignore
        | _ -> ()
        
        remotes.ToArray()
    
    let listRemotes (repo: Repo) : Result<RemoteRepo[], string> =
        try
            let configFile = Repository.getConfigFile repo
            if not (File.Exists configFile) then
                Ok [||]
            else
                let content = File.ReadAllText configFile
                Ok (parseRemoteConfig content)
        with
        | ex -> Error $"Failed to list remotes: {ex.Message}"
    
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
            while pos < body.Length do
                match PktLine.read body pos with
                | PktLine.Flush, np
                | PktLine.Delim, np ->
                    pos <- np
                | PktLine.Data d, np ->
                    pos <- np
                    let line = Encoding.UTF8.GetString(d).TrimEnd('\n')
                    if not (line.StartsWith("# service=", StringComparison.Ordinal)) then
                        let nul = line.IndexOf('\000')
                        let refPart = if nul >= 0 then line.Substring(0, nul) else line
                        let caps = if nul >= 0 then line.Substring(nul + 1) else ""
                        if caps <> "" then
                            for cap in caps.Split(' ', StringSplitOptions.RemoveEmptyEntries) do
                                if cap.StartsWith("symref=HEAD:", StringComparison.Ordinal) then
                                    headSymref <- Some(cap.Substring("symref=HEAD:".Length))
                        let pieces = refPart.Split(' ', StringSplitOptions.RemoveEmptyEntries)
                        if pieces.Length >= 2 then
                            let sha = pieces.[0].ToLowerInvariant()
                            let name = pieces.[1]
                            if sha <> zeroId && validHash sha && name <> "HEAD" && not (name.EndsWith("^{}", StringComparison.Ordinal)) then
                                refs.[name] <- sha
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

    let private listLooseRefs (repo: Repo) =
        let refsRoot = Repository.getRefsDir repo
        if not (Directory.Exists refsRoot) then []
        else
            Directory.GetFiles(refsRoot, "*", SearchOption.AllDirectories)
            |> Array.choose (fun path ->
                let rel = Path.GetRelativePath(repo.GitDir, path).Replace('\\', '/')
                if rel.EndsWith(".lock", StringComparison.Ordinal) then None
                else
                    let value = File.ReadAllText(path).Trim()
                    if validHash value then Some(rel, value.ToLowerInvariant()) else None)
            |> Array.toList

    let private listLocalRefs (repo: Repo) =
        let packed =
            match References.readPackedRefs repo with
            | Ok refs ->
                refs
                |> Array.choose (fun (name, hash) ->
                    if name.EndsWith("^{}", StringComparison.Ordinal) then None
                    elif validHash hash then Some(name, hash.ToLowerInvariant())
                    else None)
                |> Array.toList
            | Error _ -> []
        (packed @ listLooseRefs repo)
        |> List.fold (fun acc (name, hash) -> Map.add name hash acc) Map.empty

    let private writeFullRef (repo: Repo) (name: string) (hash: GitHash) =
        if name.StartsWith("refs/", StringComparison.Ordinal) then
            let path = Path.Combine(repo.GitDir, name.Replace('/', Path.DirectorySeparatorChar))
            Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
            File.WriteAllText(path, hash + "\n")

    let private removeFromPackedRefs (repo: Repo) (names: Set<string>) =
        let packed = Repository.getPackedRefsFile repo
        if File.Exists packed && not (Set.isEmpty names) then
            let lines = File.ReadAllLines packed
            let kept =
                lines
                |> Array.filter (fun line ->
                    let trimmed = line.Trim()
                    if trimmed = "" || trimmed.StartsWith("#", StringComparison.Ordinal) || trimmed.StartsWith("^", StringComparison.Ordinal) then true
                    else
                        let parts = trimmed.Split(' ', 2)
                        parts.Length < 2 || not (Set.contains parts.[1] names))
            File.WriteAllLines(packed, kept)

    let private deleteFullRef (repo: Repo) (name: string) =
        if name.StartsWith("refs/", StringComparison.Ordinal) then
            let path = Path.Combine(repo.GitDir, name.Replace('/', Path.DirectorySeparatorChar))
            if File.Exists path then File.Delete path

    let private pkt (s: string) = PktLine.encodeStr s

    let private buildUploadPackRequest (wants: GitHash list) (haves: GitHash list) =
        use ms = new MemoryStream()
        let caps = "multi_ack_detailed side-band-64k ofs-delta agent=fsgit-client/0.1"
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

    let private fetchPack (repo: Repo) (remoteUrl: string) (auth: HttpAuth option) (wants: GitHash list) (haves: GitHash list) =
        task {
            if List.isEmpty wants then
                return Ok(0, 0L)
            else
                use client = new HttpClient()
                let reqBody = buildUploadPackRequest wants haves
                use req = new HttpRequestMessage(HttpMethod.Post, serviceUrl remoteUrl "/git-upload-pack")
                applyAuth req auth
                req.Content <- new ByteArrayContent(reqBody)
                req.Content.Headers.ContentType <- MediaTypeHeaderValue("application/x-git-upload-pack-request")
                req.Headers.Accept.Add(MediaTypeWithQualityHeaderValue("application/x-git-upload-pack-result"))
                let! res = httpBytes client req
                match res with
                | Error e -> return Error e
                | Ok body ->
                    match extractPack body with
                    | Error e -> return Error e
                    | Ok (packBytes, _) ->
                        if packBytes.Length = 0 then
                            return Ok(0, 0L)
                        else
                            use input = new MemoryStream(packBytes)
                            let counting = new PackData.CountingStream(input)
                            match PackWriter.unpackPackStream repo counting with
                            | Ok written -> return Ok(written.Length, int64 packBytes.Length)
                            | Error e -> return Error e
        }

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
            let validRefName (name: string) =
                name.StartsWith("refs/", StringComparison.Ordinal) &&
                not (name.Contains("..", StringComparison.Ordinal)) &&
                not (name.Contains("\\", StringComparison.Ordinal))
            if sourceWildcards = 0 && destinationWildcards = 0 && validRefName source && validRefName destination then
                Ok(Exact(source, destination))
            elif sourceWildcards = 1 && destinationWildcards = 1 &&
                 source.EndsWith("/*", StringComparison.Ordinal) &&
                 destination.EndsWith("/*", StringComparison.Ordinal) &&
                 validRefName (source.Substring(0, source.Length - 1)) &&
                 validRefName (destination.Substring(0, destination.Length - 1)) then
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
                        match mapped.TryGetValue name with
                        | true, existing when existing <> hash ->
                            conflict <- Some(sprintf "Multiple remote refs map to %s" name)
                        | _ ->
                            mapped.[name] <- hash
            let pruneAppliesTo name =
                parsed
                |> Seq.exists (function
                    | Exact(_, destination) -> name = destination
                    | Prefix(_, destinationPrefix) -> name.StartsWith(destinationPrefix, StringComparison.Ordinal))
            match conflict with
            | Some e -> Error e
            | None ->
                Ok(mapped |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq, pruneAppliesTo)

    let private updateHeadSymref (repo: Repo) (headSymref: string option) (remoteRefs: Map<string, GitHash>) =
        match headSymref with
        | Some target when target.StartsWith("refs/", StringComparison.Ordinal) && Map.containsKey target remoteRefs ->
            References.updateHead repo (Symbolic target) |> ignore
        | _ -> ()

    let private fetchMirrorHttp (repo: Repo) (remoteUrl: string) (auth: HttpAuth option) (refspecs: string list) (prune: bool) =
        task {
            let! adv = advertiseUploadPack remoteUrl auth
            match adv with
            | Error e -> return Error e
            | Ok (advertisedRefs, headSymref) ->
                match refsForRefspecs advertisedRefs refspecs with
                | Error e -> return Error e
                | Ok (remoteRefs, pruneAppliesTo) ->
                    let localRefs = listLocalRefs repo
                    let wants =
                        remoteRefs
                        |> Map.toSeq
                        |> Seq.map snd
                        |> Seq.distinct
                        |> Seq.filter (fun h -> not (ReadObjects.objectExists repo h))
                        |> Seq.toList
                    let haves =
                        localRefs
                        |> Map.toSeq
                        |> Seq.map snd
                        |> Seq.distinct
                        |> Seq.toList
                    let! packRes = fetchPack repo remoteUrl auth wants haves
                    match packRes with
                    | Error e -> return Error e
                    | Ok (objectCount, packBytes) ->
                        let updated = ResizeArray<string * GitHash>()
                        for KeyValue(name, hash) in remoteRefs do
                            if Map.tryFind name localRefs <> Some hash then
                                writeFullRef repo name hash
                                updated.Add(name, hash)
                        let pruned =
                            if prune then
                                localRefs
                                |> Map.toSeq
                                |> Seq.map fst
                                |> Seq.filter (fun name -> pruneAppliesTo name && not (Map.containsKey name remoteRefs))
                                |> Seq.toList
                            else []
                        for name in pruned do deleteFullRef repo name
                        removeFromPackedRefs repo (Set.ofList pruned)
                        updateHeadSymref repo headSymref remoteRefs
                        return Ok {
                            RemoteRefs = remoteRefs
                            UpdatedRefs = List.ofSeq updated
                            PrunedRefs = pruned
                            ObjectCount = objectCount
                            PackBytes = packBytes
                        }
        }

    /// Fetch a mirror through a caller-supplied authenticated transport.
    /// The callbacks exchange ordinary smart-HTTP advertisement and
    /// upload-pack payload bytes without imposing HTTP as the wire transport.
    let fetchMirrorWithTransport
        (repo: Repo)
        (advertise: unit -> Result<byte[], string>)
        (uploadPack: byte[] -> Result<byte[], string>)
        (refspecs: string list)
        (prune: bool) : Result<FetchReport, string> =
        let finish remoteRefs headSymref pruneAppliesTo localRefs objectCount packByteCount =
            let updated = ResizeArray<string * GitHash>()
            for KeyValue(name, hash) in remoteRefs do
                if Map.tryFind name localRefs <> Some hash then
                    writeFullRef repo name hash
                    updated.Add(name, hash)
            let pruned =
                if prune then
                    localRefs
                    |> Map.toSeq
                    |> Seq.map fst
                    |> Seq.filter (fun name -> pruneAppliesTo name && not (Map.containsKey name remoteRefs))
                    |> Seq.toList
                else
                    []
            for name in pruned do deleteFullRef repo name
            removeFromPackedRefs repo (Set.ofList pruned)
            updateHeadSymref repo headSymref remoteRefs
            {
                RemoteRefs = remoteRefs
                UpdatedRefs = List.ofSeq updated
                PrunedRefs = pruned
                ObjectCount = objectCount
                PackBytes = packByteCount
            }

        match advertise () with
        | Error error -> Error error
        | Ok advertisement ->
            match parseAdvertisedRefs advertisement with
            | Error error -> Error error
            | Ok (advertisedRefs, headSymref) ->
                match refsForRefspecs advertisedRefs refspecs with
                | Error error -> Error error
                | Ok (remoteRefs, pruneAppliesTo) ->
                    let localRefs = listLocalRefs repo
                    let wants =
                        remoteRefs
                        |> Map.toSeq
                        |> Seq.map snd
                        |> Seq.distinct
                        |> Seq.filter (fun hash -> not (ReadObjects.objectExists repo hash))
                        |> Seq.toList
                    let haves =
                        localRefs
                        |> Map.toSeq
                        |> Seq.map snd
                        |> Seq.distinct
                        |> Seq.toList
                    if List.isEmpty wants then
                        Ok(finish remoteRefs headSymref pruneAppliesTo localRefs 0 0L)
                    else
                        match uploadPack (buildUploadPackRequest wants haves) with
                        | Error error -> Error error
                        | Ok response ->
                            match extractPack response with
                            | Error error -> Error error
                            | Ok (packBytes, _) when packBytes.Length = 0 ->
                                Ok(finish remoteRefs headSymref pruneAppliesTo localRefs 0 0L)
                            | Ok (packBytes, _) ->
                                use input = new MemoryStream(packBytes)
                                let counting = new PackData.CountingStream(input)
                                match PackWriter.unpackPackStream repo counting with
                                | Error error -> Error error
                                | Ok written ->
                                    Ok(finish remoteRefs headSymref pruneAppliesTo localRefs written.Length (int64 packBytes.Length))

    let fetchMirrorWithAuth (repo: Repo) (remoteUrl: string) (auth: HttpAuth option) (refspecs: string list) (prune: bool) : Result<FetchReport, string> =
        if not (remoteUrl.StartsWith("http://", StringComparison.OrdinalIgnoreCase) || remoteUrl.StartsWith("https://", StringComparison.OrdinalIgnoreCase)) then
            Error "Only HTTP(S) mirror fetch is implemented"
        else
            fetchMirrorHttp repo remoteUrl auth refspecs prune
            |> Async.AwaitTask
            |> Async.RunSynchronously

    let fetchMirror (repo: Repo) (remoteUrl: string) (refspecs: string list) (prune: bool) : Result<FetchReport, string> =
        fetchMirrorWithAuth repo remoteUrl None refspecs prune

    let lsRemoteHttp (remoteUrl: string) (auth: HttpAuth option) : Result<Map<string, GitHash>, string> =
        advertiseUploadPack remoteUrl auth
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> Result.map fst

    let private gitProtocolFetch (remoteUrl: string) (refs: string[]) : Result<Map<string, GitHash>, string> =
        Error "Git protocol fetch not yet implemented"
    
    let private httpFetch (remoteUrl: string) (refs: string[]) : Result<Map<string, GitHash>, string> =
        lsRemoteHttp remoteUrl None
    
    let private sshFetch (remoteUrl: string) (refs: string[]) : Result<Map<string, GitHash>, string> =
        Error "SSH fetch not yet implemented"
    
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
                Ok ()
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
    
    let addRemote (repo: Repo) (name: string) (url: string) : Result<unit, string> =
        try
            let configFile = Repository.getConfigFile repo
            let mutable content = if File.Exists configFile then File.ReadAllText configFile else ""
            
            let section = $"\n[remote \"{name}\"]\n\turl = {url}\n"
            if content.Contains($"[remote \"{name}\"]") then
                Error $"Remote {name} already exists"
            else
                content <- content + section
                File.WriteAllText(configFile, content)
                Ok ()
        with
        | ex -> Error $"Failed to add remote: {ex.Message}"
    
    let removeRemote (repo: Repo) (name: string) : Result<unit, string> =
        try
            let configFile = Repository.getConfigFile repo
            if not (File.Exists configFile) then
                Ok ()
            else
                let content = File.ReadAllText configFile
                let startTag = $"[remote \"{name}\"]"
                let startIndex = content.IndexOf startTag
                if startIndex < 0 then
                    Error $"Remote {name} not found"
                else
                    let endIndex = content.IndexOf("\n[", startIndex + 1)
                    let newContent =
                        if endIndex > 0 then
                            content.[0..startIndex - 1] + content.[endIndex..]
                        else
                            content.[0..startIndex - 1]
                    File.WriteAllText(configFile, newContent)
                    Ok ()
        with
        | ex -> Error $"Failed to remove remote: {ex.Message}"
