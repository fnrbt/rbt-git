module ProtocolRegression

open System
open System.IO
open System.Diagnostics
open System.Net
open System.Net.Sockets
open System.Threading
open System.Threading.Tasks
open System.Text
open Rbt.Git

let private check ok message = if not ok then failwith message
let private value = function Ok x -> x | Error error -> failwith error
let private zero = String('0', 40)
let private signature = { Name = "Protocol test"; Email = "protocol@example.invalid"; Time = DateTimeOffset.FromUnixTimeSeconds(1L) }

let private withRepos action =
    let root = Path.Combine(Path.GetTempPath(), "rbt-protocol-" + Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory root |> ignore
    try
        let local = Repository.initBare(Path.Combine(root, "local.git")) |> value
        let remote = Repository.initBare(Path.Combine(root, "remote.git")) |> value
        action local remote
    finally
        Directory.Delete(root, true)

let private commit repo parent (message: string) =
    let blob = ObjectWriter.writeBlob repo (Encoding.UTF8.GetBytes message) |> value
    let tree = ObjectWriter.writeTree repo [| { Mode = 0o100644; Path = "file"; Hash = blob } |] |> value
    let commit =
        ObjectWriter.writeCommit repo
            { Tree = tree; Parents = parent |> Option.toArray
              Author = signature; Committer = signature; Message = message }
        |> value
    commit, [| blob; tree; commit |]

let private receive destination source oldTip newTip refName (objects: GitHash[]) =
    use request = new MemoryStream()
    PktLine.writeStr request $"{oldTip} {newTip} {refName}\000report-status\n"
    PktLine.writeFlush request
    if objects.Length > 0 then
        let pack = PackWriter.writePackFor source objects |> value
        request.Write(pack, 0, pack.Length)
    SmartHttp.receivePack destination (request.ToArray()) |> Encoding.UTF8.GetString

let private advertisement entries =
    use stream = new MemoryStream()
    PktLine.writeStr stream "# service=git-upload-pack\n"
    PktLine.writeFlush stream
    for (hash, name) in entries do PktLine.writeStr stream $"{hash} {name}\n"
    PktLine.writeFlush stream
    stream.ToArray()

let private mirror local entries upload prune =
    Remote.fetchMirrorWithTransport local (fun () -> Ok(advertisement entries)) upload [] prune

let private framing () =
    for header in [ "0002"; "0003"; "000g"; "0005"; "fff0"; "000" ] do
        let bytes = Encoding.ASCII.GetBytes header
        let rejected =
            try PktLine.read bytes 0 |> ignore; false
            with :? InvalidDataException -> true
        check rejected $"pkt-line accepted malformed framing {header}"
    use input = new MemoryStream(Encoding.ASCII.GetBytes "0008ab")
    let rejected =
        try PktLine.readFrom input |> ignore; false
        with :? InvalidDataException -> true
    check rejected "stream packet accepted truncated payload"

let private hostileAdvertisements () =
    withRepos (fun local _ ->
        for name in [ "refs/heads/../../escape"; "refs/heads/.hidden"; "refs/heads/a.lock"; "refs/heads/evil\\name" ] do
            let result = mirror local [ zero, name ] (fun _ -> failwith "hostile advertisement requested objects") true
            check (Result.isError result) $"accepted hostile advertised ref {name}"
        check (not (File.Exists(Path.Combine(Path.GetDirectoryName local.GitDir, "escape")))) "hostile advertisement escaped git directory")

let private fetchConnectivity () =
    withRepos (fun local remote ->
        let target, objects = commit remote None "remote"
        let refs = [ target, "refs/heads/main" ]
        check (Result.isError (mirror local refs (fun _ -> Ok(PktLine.encodeStr "NAK\n")) true)) "NAK-only fetch succeeded"
        check (SmartHttp.resolveFull local "refs/heads/main" 0 = None) "NAK-only fetch changed refs"
        let unrelated, unrelatedObjects = commit remote None "unrelated"
        let unrelatedPack = PackWriter.writePackFor remote unrelatedObjects |> value
        check (Result.isError (mirror local refs (fun _ -> Ok(Array.append (PktLine.encodeStr "NAK\n") unrelatedPack)) true)) "unrelated pack succeeded"
        let missingPack = PackWriter.writePackFor remote [| objects.[2] |] |> value
        check (Result.isError (mirror local refs (fun _ -> Ok(Array.append (PktLine.encodeStr "NAK\n") missingPack)) true)) "incomplete pack succeeded"
        check (SmartHttp.resolveFull local "refs/heads/main" 0 = None) "incomplete fetch changed refs"
        check (not (ReadObjects.objectExists local target)) "rejected incomplete pack escaped fetch quarantine"
        check (not (ReadObjects.objectExists local unrelated)) "rejected unrelated pack escaped fetch quarantine"
        check (ReadObjects.objectExists remote unrelated) "source object missing")

let private rejectedFetchCannotPoisonReceive () =
    withRepos (fun local remote ->
        let broken =
            ObjectWriter.writeCommit remote
                { Tree = String.replicate 40 "a"; Parents = [||];
                  Author = signature; Committer = signature; Message = "missing tree" }
            |> value
        let brokenPack = PackWriter.writePackFor remote [| broken |] |> value
        let failedFetch =
            mirror local [ broken, "refs/heads/poison" ]
                (fun _ -> Ok(Array.append (PktLine.encodeStr "NAK\n") brokenPack)) false
        check (Result.isError failedFetch) "malformed fetch succeeded"
        check (not (ReadObjects.objectExists local broken)) "malformed fetch committed object before validation"
        let child, childObjects = commit remote (Some broken) "child"
        let pushResponse = receive local remote zero child "refs/heads/child" childObjects
        check
            (pushResponse.Contains("unpack ", StringComparison.Ordinal) &&
             not (pushResponse.Contains("unpack ok\n", StringComparison.Ordinal)))
            "receive trusted a rejected fetch object"
        check (SmartHttp.resolveFull local "refs/heads/child" 0 = None) "receive published a child of rejected fetch")

let private defaultRefspecIncludesTwoComponentRefs () =
    withRepos (fun local remote ->
        let blob = ObjectWriter.writeBlob remote (Encoding.UTF8.GetBytes "two-component ref") |> value
        let pack = PackWriter.writePackFor remote [| blob |] |> value
        let report =
            mirror local [ blob, "refs/foo" ]
                (fun _ -> Ok(Array.append (PktLine.encodeStr "NAK\n") pack)) false
            |> value
        check (List.contains ("refs/foo", blob) report.UpdatedRefs) "default mirror refspec omitted refs/foo"
        check (SmartHttp.resolveFull local "refs/foo" 0 = Some blob) "default mirror refspec failed to publish refs/foo")

let private fetchRejectsConcurrentRefUpdate () =
    withRepos (fun local remote ->
        let oldTip, _ = commit local None "old"
        let externalTip, _ = commit local None "external"
        let remoteTip, objects = commit remote None "remote"
        References.compareAndSwap local "refs/heads/main" None (Some oldTip) |> value
        let pack = PackWriter.writePackFor remote objects |> value
        let result =
            mirror local [ remoteTip, "refs/heads/main" ]
                (fun _ ->
                    References.compareAndSwap local "refs/heads/main" (Some oldTip) (Some externalTip) |> value
                    Ok(Array.append (PktLine.encodeStr "NAK\n") pack)) false
        check (Result.isError result) "fetch overwrote a concurrent ref update"
        check (SmartHttp.resolveFull local "refs/heads/main" 0 = Some externalTip) "external ref update was lost")

let private packedRefsAndBranchType () =
    withRepos (fun local remote ->
        let target, objects = commit remote None "packed"
        let packed = Repository.getPackedRefsFile local
        File.WriteAllText(packed, $"# pack-refs with: peeled fully-peeled\n{target} refs/tags/old\n^{target}\n")
        let report = mirror local [] (fun _ -> failwith "empty fetch requested a pack") true |> value
        check (List.contains "refs/tags/old" report.PrunedRefs) "packed ref not pruned"
        check (not ((File.ReadAllText packed).Contains("^"))) "orphan packed peeled record remains"
        let blob = objects.[0]
        let blobPack = PackWriter.writePackFor remote [| blob |] |> value
        let result = mirror local [ blob, "refs/heads/main" ] (fun _ -> Ok(Array.append (PktLine.encodeStr "NAK\n") blobPack)) false
        check (Result.isError result) "branch accepted blob target"
        check (SmartHttp.resolveFull local "refs/heads/main" 0 = None) "invalid branch type was published"
        check (not (ReadObjects.objectExists local blob)) "rejected blob branch escaped fetch quarantine")

let private uploadFailureIsResult () =
    withRepos (fun _ remote ->
        let target, objects = commit remote None "missing subtree"
        References.compareAndSwap remote "refs/heads/main" None (Some target) |> value
        File.Delete(Repository.getLooseObjectPath remote objects.[1])
        use request = new MemoryStream()
        PktLine.writeStr request $"want {target}\n"
        PktLine.writeFlush request
        PktLine.writeStr request "done\n"
        let bytes = request.ToArray()
        use output = new MemoryStream()
        check (Result.isError (SmartHttp.uploadPackTo remote bytes output)) "uploadPackTo leaked missing-object failure"
        use policyOutput = new MemoryStream()
        check (Result.isError (SmartHttp.uploadPackToForPolicy remote RefPolicy.Public bytes policyOutput))
            "policy upload-pack leaked missing-object failure"
        use streamOutput = new MemoryStream()
        check (Result.isError (SmartHttp.uploadPackStream remote (new MemoryStream(bytes)) streamOutput))
            "direct upload-pack leaked missing-object failure"
        use v2Request = new MemoryStream()
        PktLine.writeStr v2Request "command=fetch\n"
        PktLine.writeDelim v2Request
        PktLine.writeStr v2Request $"want {target}\n"
        PktLine.writeStr v2Request "done\n"
        PktLine.writeFlush v2Request
        use v2Output = new MemoryStream()
        check (Result.isError (SmartHttp.uploadPackV2To remote (v2Request.ToArray()) v2Output))
            "v2 upload-pack leaked missing-object failure")

let private receiveQuarantine () =
    withRepos (fun local remote ->
        let good, goodObjects = commit remote None "good"
        let missingTree = String.replicate 40 "a"
        let broken =
            ObjectWriter.writeCommit remote
                { Tree = missingTree; Parents = [||]; Author = signature; Committer = signature; Message = "broken" }
            |> value
        let badResponse = receive local remote zero broken "refs/heads/main" [| broken |]
        check (badResponse.Contains("unpack ", StringComparison.Ordinal) && not (badResponse.Contains("unpack ok\n", StringComparison.Ordinal))) "incomplete push was accepted"
        check (not (ReadObjects.objectExists local broken)) "rejected objects escaped quarantine"
        let goodResponse = receive local remote zero good "refs/heads/main" goodObjects
        check (goodResponse.Contains "ok refs/heads/main") $"valid push following rejected push failed: {goodResponse}"
        let blob = goodObjects.[0]
        let rejected = receive local remote zero blob "refs/heads/not-a-commit" [| blob |]
        check (rejected.Contains("ng refs/heads/not-a-commit ", StringComparison.Ordinal)) "branch accepted blob target"
        check (SmartHttp.resolveFull local "refs/heads/not-a-commit" 0 = None) "invalid branch target was published")

let private packedReceiveAndHostileCommand () =
    withRepos (fun local remote ->
        let oldTip, _ = commit local None "old"
        let newTip, objects = commit remote (Some oldTip) "new"
        File.WriteAllText(Repository.getPackedRefsFile local, $"{oldTip} refs/heads/main\n")
        let invalid = receive local remote zero newTip "refs/heads/../../outside" objects
        check (invalid.Contains("unpack ", StringComparison.Ordinal) && not (invalid.Contains("unpack ok\n", StringComparison.Ordinal))) "hostile receive ref was accepted"
        check (not (ReadObjects.objectExists local newTip)) "hostile receive promoted its pack"
        let accepted = receive local remote oldTip newTip "refs/heads/main" objects
        check (accepted.Contains "ok refs/heads/main") $"packed branch update failed: {accepted}"
        check (SmartHttp.resolveFull local "refs/heads/main" 0 = Some newTip) "packed branch was not updated")

let private snapshotsAndNegotiation () =
    withRepos (fun local remote ->
        let first, _ = commit remote None "first"
        let last, _ = commit remote (Some first) "last"
        File.WriteAllText(Repository.getPackedRefsFile remote, $"{last} refs/heads/main\n")
        References.updateHead remote (Symbolic "refs/heads/main") |> value
        let advertisement = SmartHttp.advertiseRefs remote "git-upload-pack" |> Encoding.UTF8.GetString
        check (advertisement.Contains(last + " HEAD")) "packed HEAD not advertised"
        let publicAdvertisement = SmartHttp.advertiseRefsForPolicy remote "git-upload-pack" RefPolicy.Public |> value |> Encoding.UTF8.GetString
        check (publicAdvertisement.Contains(last + " HEAD")) "policy HEAD not resolved from packed refs"
        References.updateHead remote (Symbolic "refs/internal/secret") |> value
        References.compareAndSwap remote "refs/internal/secret" None (Some last) |> value
        let filtered = SmartHttp.advertiseRefsForPolicy remote "git-upload-pack" RefPolicy.Public |> value |> Encoding.UTF8.GetString
        check (not (filtered.Contains "symref=HEAD:refs/internal/secret")) "policy leaked hidden HEAD target"
        use lsRequest = new MemoryStream()
        PktLine.writeStr lsRequest "command=ls-refs\n"
        PktLine.writeDelim lsRequest
        PktLine.writeStr lsRequest "symrefs\n"
        PktLine.writeStr lsRequest "unborn\n"
        PktLine.writeFlush lsRequest
        use lsResponse = new MemoryStream()
        SmartHttp.uploadPackV2To remote (lsRequest.ToArray()) lsResponse |> value
        check (not ((Encoding.UTF8.GetString(lsResponse.ToArray())).Contains "HEAD")) "v2 ls-refs leaked hidden HEAD"
        use request = new MemoryStream()
        PktLine.writeStr request $"want {last} shallow\n"
        PktLine.writeFlush request
        PktLine.writeStr request $"shallow {last}\n"
        PktLine.writeStr request "deepen 2\n"
        PktLine.writeFlush request
        PktLine.writeStr request "done\n"
        use response = new MemoryStream()
        SmartHttp.uploadPackStream remote (new MemoryStream(request.ToArray())) response |> value
        let data = Encoding.UTF8.GetString(response.ToArray())
        check (data.Contains("unshallow " + last)) "direct deepening did not unshallow old boundary"
        check (data.Contains "PACK") "direct deepening did not transfer pack"
        use v2Request = new MemoryStream()
        PktLine.writeStr v2Request "command=fetch\n"
        PktLine.writeDelim v2Request
        PktLine.writeStr v2Request $"want {last}\n"
        PktLine.writeStr v2Request $"shallow {last}\n"
        PktLine.writeStr v2Request "deepen 2\n"
        PktLine.writeStr v2Request "done\n"
        PktLine.writeFlush v2Request
        use v2Response = new MemoryStream()
        SmartHttp.uploadPackV2To remote (v2Request.ToArray()) v2Response |> value
        let v2Wire = Encoding.UTF8.GetString(v2Response.ToArray())
        check (v2Wire.Contains("unshallow " + last)) "v2 deepening did not unshallow old boundary"
        check (v2Wire.Contains "packfile") "v2 deepening omitted packfile section"

        use negotiate = new MemoryStream()
        PktLine.writeStr negotiate $"want {last}\n"
        PktLine.writeFlush negotiate
        PktLine.writeStr negotiate $"have {first}\n"
        PktLine.writeFlush negotiate
        PktLine.writeStr negotiate "done\n"
        use answer = new MemoryStream()
        SmartHttp.uploadPackStream remote (new MemoryStream(negotiate.ToArray())) answer |> value
        let wire = answer.ToArray()
        let firstReply, afterFirst = PktLine.read wire 0
        let secondReply, _ = PktLine.read wire afterFirst
        check (firstReply = PktLine.Data(Encoding.UTF8.GetBytes "NAK\n")) "negotiation flush did not yield NAK"
        check (secondReply = PktLine.Data(Encoding.UTF8.GetBytes "NAK\n")) "done boundary did not yield final NAK")

let private remoteConfiguration () =
    withRepos (fun local remote ->
        let url = "https://example.invalid/repo.git?key=a=b#fragment path"
        Remote.addRemote local "equal" url |> value
        GitConfig.updateValue local "core" None "description" "unrelated value" |> value
        let configured = Remote.getRemote local "equal" |> value |> Option.get
        check (configured.FetchUrl = url) "canonical config update changed a URL with =/#/spaces"
        Remote.addRemote local "file" remote.GitDir |> value
        check (Result.isError (Remote.fetch local "file")) "file fetch reported success without transferring objects"
        Remote.addRemote local "peer" "https://initial.invalid/path" |> value
        GitConfig.updateConfig local (fun config ->
            Ok { Sections =
                Array.append config.Sections
                    [| { Name = "remote"; Subsection = Some "peer";
                         Entries = [| "pushurl", "ssh://example.invalid/push#a=b"; "url", "https://later.invalid/a=b#two" |] }
                       { Name = "core"; Subsection = None;
                         Entries = [| "repeated", "one"; "repeated", "two" |] } |] })
        |> value
        let peers = Remote.listRemotes local |> value |> Array.filter (fun r -> r.Name = "peer")
        check (peers.Length = 1) "repeated remote sections created duplicate remotes"
        check (peers.[0].FetchUrl = "https://later.invalid/a=b#two") "repeated remote sections lost last URL"
        check (peers.[0].PushUrl = Some "ssh://example.invalid/push#a=b") "pushurl did not override fetch URL"
        Remote.removeRemote local "peer" |> value
        let config = GitConfig.readConfig local |> value
        check (not (GitConfig.hasSection config "remote" (Some "peer"))) "remote removal left repeated sections"
        check (GitConfig.getValues config "core" None "repeated" = [| "one"; "two" |]) "remote removal lost unrelated repeated entries"
        check ((Remote.getRemote local "equal" |> value |> Option.get).FetchUrl = url) "remote removal changed unrelated URL"
        let quotedName = "quote\"name"
        Remote.addRemote local quotedName url |> value
        check ((Remote.getRemote local quotedName |> value |> Option.get).FetchUrl = url) "escaped remote subsection failed to round-trip"
        Remote.removeRemote local quotedName |> value
        check (Result.isError (Remote.addRemote local "../escape" url)) "unsafe remote name was accepted")

let private gitShallowInteroperability () =
    withRepos (fun _ remote ->
        let first, _ = commit remote None "first"
        let last, _ = commit remote (Some first) "last"
        References.compareAndSwap remote "refs/heads/main" None (Some last) |> value
        References.updateHead remote (Symbolic "refs/heads/main") |> value
        use reservation = new TcpListener(IPAddress.Loopback, 0)
        reservation.Start()
        let port = (reservation.LocalEndpoint :?> IPEndPoint).Port
        reservation.Stop()
        use listener = new HttpListener()
        listener.Prefixes.Add($"http://127.0.0.1:{port}/")
        listener.Start()
        use cancellation = new CancellationTokenSource()
        let traffic = ResizeArray<string>()
        let record message = lock traffic (fun () -> traffic.Add message)
        let trafficSummary () = lock traffic (fun () -> String.concat " | " traffic)
        let server = task {
            try
                while not cancellation.IsCancellationRequested do
                    let! context = listener.GetContextAsync().WaitAsync(cancellation.Token)
                    try
                        let path = context.Request.Url.AbsolutePath
                        record (context.Request.HttpMethod + " " + path)
                        if context.Request.HttpMethod = "GET" && path.EndsWith("/info/refs", StringComparison.Ordinal) then
                            let response = SmartHttp.advertiseRefs remote "git-upload-pack"
                            context.Response.ContentType <- "application/x-git-upload-pack-advertisement"
                            context.Response.ContentLength64 <- int64 response.Length
                            do! context.Response.OutputStream.WriteAsync(response)
                        elif context.Request.HttpMethod = "POST" && path.EndsWith("/git-upload-pack", StringComparison.Ordinal) then
                            use request = new MemoryStream()
                            do! context.Request.InputStream.CopyToAsync(request)
                            let text = Encoding.UTF8.GetString(request.ToArray())
                            record ("upload request: " + text)
                            match SmartHttp.uploadPack remote (request.ToArray()) with
                            | Error error -> failwith error
                            | Ok response ->
                                context.Response.ContentType <- "application/x-git-upload-pack-result"
                                context.Response.ContentLength64 <- int64 response.Length
                                do! context.Response.OutputStream.WriteAsync(response)
                                record $"upload response: {response.Length} bytes"
                        else context.Response.StatusCode <- 404
                    finally context.Response.Close()
            with
            | :? OperationCanceledException -> ()
            | :? HttpListenerException when cancellation.IsCancellationRequested -> ()
        }
        let runGit cwd arguments =
            use gitProcess = new Process()
            let start = ProcessStartInfo("git", WorkingDirectory = cwd, UseShellExecute = false,
                                         RedirectStandardOutput = true, RedirectStandardError = true)
            for argument in arguments do start.ArgumentList.Add argument
            start.Environment["GIT_TERMINAL_PROMPT"] <- "0"
            start.Environment["GIT_TRACE_PACKET"] <- "1"
            gitProcess.StartInfo <- start
            gitProcess.Start() |> ignore
            let stdoutTask = gitProcess.StandardOutput.ReadToEndAsync()
            let stderrTask = gitProcess.StandardError.ReadToEndAsync()
            let completion = Task.WhenAll([| stdoutTask :> Task; stderrTask :> Task; gitProcess.WaitForExitAsync() |])
            try completion.WaitAsync(TimeSpan.FromSeconds(30.0)).GetAwaiter().GetResult()
            with :? TimeoutException ->
                if not gitProcess.HasExited then gitProcess.Kill(true)
                if not (gitProcess.WaitForExit(5000)) then
                    failwithf "git %s could not be stopped; HTTP: %s"
                        (String.concat " " arguments) (trafficSummary ())
                let trace =
                    try stderrTask.WaitAsync(TimeSpan.FromSeconds(5.0)).GetAwaiter().GetResult()
                    with :? TimeoutException -> "git stderr stream did not close"
                failwithf "git %s timed out; HTTP: %s; git trace: %s; server: %A"
                    (String.concat " " arguments) (trafficSummary ()) trace
                    (if server.IsFaulted then server.Exception else null)
            if gitProcess.ExitCode <> 0 then
                failwithf "git %s failed: %s; HTTP: %s"
                    (String.concat " " arguments) (stderrTask.GetAwaiter().GetResult()) (trafficSummary ())
            stdoutTask.GetAwaiter().GetResult().Trim()
        try
            let root = Path.GetDirectoryName remote.GitDir
            let client = Path.Combine(root, "shallow-client")
            let url = $"http://127.0.0.1:{port}/remote.git"
            runGit root [ "-c"; "protocol.version=0"; "clone"; "--depth=1"; "--no-tags"; url; client ] |> ignore
            check (runGit client [ "rev-list"; "--count"; "HEAD" ] = "1") "real Git did not receive a shallow clone"
            runGit client [ "-c"; "protocol.version=0"; "fetch"; "--deepen=1"; "origin" ] |> ignore
            check (runGit client [ "rev-list"; "--count"; "HEAD" ] = "2") "real Git could not deepen shallow history"
            check (runGit client [ "rev-parse"; "--is-shallow-repository" ] = "false") "real Git did not clear shallow boundary"
        finally
            cancellation.Cancel()
            listener.Stop()
            try server.WaitAsync(TimeSpan.FromSeconds(5.0)).GetAwaiter().GetResult()
            with :? TimeoutException -> failwithf "Git HTTP server did not stop; HTTP: %s" (trafficSummary ()))

let run () =
    let runCase name (seconds: int) (action: unit -> unit) =
        printfn "protocol case: %s" name
        let completion = Task.Run(Action(action))
        try completion.WaitAsync(TimeSpan.FromSeconds(float seconds)).GetAwaiter().GetResult()
        with :? TimeoutException -> failwithf "protocol case %s timed out after %d seconds" name seconds
    runCase "framing" 20 framing
    runCase "hostileAdvertisements" 20 hostileAdvertisements
    runCase "fetchConnectivity" 20 fetchConnectivity
    runCase "rejectedFetchCannotPoisonReceive" 20 rejectedFetchCannotPoisonReceive
    runCase "defaultRefspecIncludesTwoComponentRefs" 20 defaultRefspecIncludesTwoComponentRefs
    runCase "fetchRejectsConcurrentRefUpdate" 20 fetchRejectsConcurrentRefUpdate
    runCase "packedRefsAndBranchType" 20 packedRefsAndBranchType
    runCase "uploadFailureIsResult" 20 uploadFailureIsResult
    runCase "receiveQuarantine" 20 receiveQuarantine
    runCase "snapshotsAndNegotiation" 20 snapshotsAndNegotiation
    runCase "packedReceiveAndHostileCommand" 20 packedReceiveAndHostileCommand
    runCase "remoteConfiguration" 20 remoteConfiguration
    runCase "gitShallowInteroperability" 90 gitShallowInteroperability
