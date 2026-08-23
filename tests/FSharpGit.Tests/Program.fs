open System
open System.IO
open System.Text
open System.Diagnostics
open System.Net
open System.Net.Sockets
open System.Threading
open System.Threading.Tasks
open FSharpGit

let check condition message =
    if not condition then failwith message

let value = function
    | Ok result -> result
    | Error error -> failwith error

let signature =
    { Name = "FSharpGit test"
      Email = "test@example.invalid"
      Time = DateTimeOffset.FromUnixTimeSeconds(1L) }

let writeCommit repo tree parents message =
    ObjectWriter.writeCommit repo {
        Tree = tree
        Parents = parents
        Author = signature
        Committer = signature
        Message = message
    }
    |> value

let sendPack destination oldTip newTip refName source packedObjects =
    let pack = PackWriter.writePackFor source packedObjects |> value
    use request = new MemoryStream()
    PktLine.writeStr request $"{oldTip} {newTip} {refName}\000report-status\n"
    PktLine.writeFlush request
    request.Write(pack, 0, pack.Length)
    SmartHttp.receivePack destination (request.ToArray())

let withRepositories test =
    let root = Path.Combine(Path.GetTempPath(), "fsgit-receive-tests-" + Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory root |> ignore
    try
        let destination = Repository.initBare (Path.Combine(root, "destination.git")) |> value
        let source = Repository.initBare (Path.Combine(root, "source.git")) |> value
        test destination source
    finally
        if Directory.Exists root then Directory.Delete(root, true)

let acceptsPushWithoutWalkingHistoricalGraph () =
    withRepositories (fun destination source ->
        let historicalBlob = ObjectWriter.writeBlob destination (Encoding.UTF8.GetBytes "historical") |> value
        let historicalTree =
            ObjectWriter.writeTree destination
                [| { Mode = 0o100644; Path = "historical.txt"; Hash = historicalBlob } |]
            |> value
        let historicalCommit = writeCommit destination historicalTree [||] "historical"

        let boundaryBlob = ObjectWriter.writeBlob destination (Encoding.UTF8.GetBytes "boundary") |> value
        let boundaryTree =
            ObjectWriter.writeTree destination
                [| { Mode = 0o100644; Path = "boundary.txt"; Hash = boundaryBlob } |]
            |> value
        let boundaryCommit = writeCommit destination boundaryTree [| historicalCommit |] "boundary"
        References.writeDirectAtomic destination "refs/heads/master" boundaryCommit |> value

        File.Delete(Repository.getLooseObjectPath destination historicalTree)

        let introducedBlob = ObjectWriter.writeBlob source (Encoding.UTF8.GetBytes "introduced") |> value
        let introducedTree =
            ObjectWriter.writeTree source
                [| { Mode = 0o100644; Path = "introduced.txt"; Hash = introducedBlob } |]
            |> value
        let introducedCommit = writeCommit source introducedTree [| boundaryCommit |] "introduced"

        let response =
            sendPack
                destination
                boundaryCommit
                introducedCommit
                "refs/heads/master"
                source
                [| introducedBlob; introducedTree; introducedCommit |]
            |> Encoding.UTF8.GetString

        check (response.Contains("unpack ok")) $"push failed: {response}"
        check
            (SmartHttp.resolveFull destination "refs/heads/master" 0 = Some introducedCommit)
            "receive-pack did not update the ref"

        let snapshot = References.snapshot destination RefPolicy.Replication |> value
        let fullReport = Fsck.verifyReachable destination snapshot |> value
        check (not fullReport.IsValid) "test corruption was not visible to full reachable fsck"

        let boundedReport =
            Fsck.verifyIntroduced
                destination
                [ introducedCommit ]
                [ introducedBlob; introducedTree; introducedCommit ]
            |> value
        check boundedReport.IsValid "bounded receive verification crossed into historical corruption"
        check (boundedReport.ObjectsChecked = 4) $"expected three introduced objects and one boundary, checked {boundedReport.ObjectsChecked}")

let rejectsMissingIntroducedGraphObject () =
    withRepositories (fun destination source ->
        let missingTree = String.replicate 40 "a"
        let malformedCommit = writeCommit source missingTree [||] "missing tree"
        let zeroGitHash = String.replicate 40 "0"

        let response =
            sendPack
                destination
                zeroGitHash
                malformedCommit
                "refs/heads/master"
                source
                [| malformedCommit |]
            |> Encoding.UTF8.GetString

        check
            (response.Contains("introduced object validation failed"))
            $"receive-pack accepted an incomplete introduced graph: {response}"
        check
            (SmartHttp.resolveFull destination "refs/heads/master" 0 = None)
            "receive-pack updated a ref to an incomplete object graph")

let streamsPackOnlyAfterClientSendsDone () =
    withRepositories (fun _ source ->
        let blob = ObjectWriter.writeBlob source (Encoding.UTF8.GetBytes "hello") |> value
        let tree =
            ObjectWriter.writeTree source
                [| { Mode = 0o100644; Path = "f.txt"; Hash = blob } |]
            |> value
        let commit = writeCommit source tree [||] "c0"
        References.writeDirectAtomic source "refs/heads/master" commit |> value

        let caps = "side-band-64k ofs-delta"
        let zeroId = String.replicate 40 "0"

        // Compute round with an unknown have: must NAK, stream no pack, and end
        // with the acknowledgment only. A trailing flush aborts real git with
        // "expected ACK/NAK, got a flush packet".
        use negotiation = new MemoryStream()
        PktLine.writeStr negotiation (sprintf "want %s %s\n" commit caps)
        PktLine.writeFlush negotiation
        PktLine.writeStr negotiation (sprintf "have %s\n" zeroId)
        PktLine.writeFlush negotiation
        let negotiationText =
            SmartHttp.uploadPack source (negotiation.ToArray()) |> value |> Encoding.UTF8.GetString
        check (negotiationText.Contains "NAK") "compute round with an unknown have must acknowledge with NAK"
        check
            (not (negotiationText.Contains "PACK"))
            "compute round must not stream a pack before the client sends done"
        check
            (not (negotiationText.Contains "packing"))
            "compute round must not emit side-band progress before the client sends done"
        check
            (not (negotiationText.Contains "0000"))
            "compute round must not append a flush after the acknowledgment"

        // Compute round with a common have: we advertise no multi_ack, so we
        // deliberately still reply "NAK" (never a bare "ACK <sha>", which would
        // put git in its "ready" state and make it expect the done round to be
        // the packfile alone), with no flush and no pack.
        use commonRound = new MemoryStream()
        PktLine.writeStr commonRound (sprintf "want %s %s\n" commit caps)
        PktLine.writeFlush commonRound
        PktLine.writeStr commonRound (sprintf "have %s\n" commit)
        PktLine.writeFlush commonRound
        let commonText =
            SmartHttp.uploadPack source (commonRound.ToArray()) |> value |> Encoding.UTF8.GetString
        check (commonText.Contains "NAK") "compute round with a common have must still reply NAK"
        check (not (commonText.Contains "ACK ")) "compute round must not send a bare ACK (no multi_ack advertised)"
        check (not (commonText.Contains "0000")) "compute round with a common have must not append a flush"
        check
            (not (commonText.Contains "PACK"))
            "compute round must not stream a pack even when a have is common"

        // Final round: the same wants + have, now with "done".
        use final = new MemoryStream()
        PktLine.writeStr final (sprintf "want %s %s\n" commit caps)
        PktLine.writeFlush final
        PktLine.writeStr final (sprintf "have %s\n" zeroId)
        PktLine.writeStr final "done\n"
        PktLine.writeFlush final
        let finalText =
            SmartHttp.uploadPack source (final.ToArray()) |> value |> Encoding.UTF8.GetString
        check (finalText.Contains "PACK") "done round must stream the packfile")


let realGitIncrementalFetchStaysIncremental () =
    withRepositories (fun _ source ->
        let large = Array.zeroCreate<byte> (2 * 1024 * 1024)
        Random(42).NextBytes large
        let largeBlob = ObjectWriter.writeBlob source large |> value
        let initialTree =
            ObjectWriter.writeTree source
                [| { Mode = 0o100644; Path = "large.bin"; Hash = largeBlob } |]
            |> value
        let initialCommit = writeCommit source initialTree [||] "initial"
        References.writeDirectAtomic source "refs/heads/master" initialCommit |> value

        use reservation = new TcpListener(IPAddress.Loopback, 0)
        reservation.Start()
        let port = (reservation.LocalEndpoint :?> IPEndPoint).Port
        reservation.Stop()
        use listener = new HttpListener()
        listener.Prefixes.Add($"http://127.0.0.1:{port}/")
        listener.Start()
        use cancellation = new CancellationTokenSource()
        let requests = ResizeArray<byte[]>()
        let responseSizes = ResizeArray<int>()
        let server = task {
            try
                while not cancellation.IsCancellationRequested do
                    let! context = listener.GetContextAsync().WaitAsync(cancellation.Token)
                    let path = context.Request.Url.AbsolutePath
                    if context.Request.HttpMethod = "GET" && path.EndsWith("/info/refs") then
                        let response = SmartHttp.advertiseRefs source "git-upload-pack"
                        context.Response.ContentType <- "application/x-git-upload-pack-advertisement"
                        context.Response.ContentLength64 <- int64 response.Length
                        do! context.Response.OutputStream.WriteAsync(response)
                    elif context.Request.HttpMethod = "POST" && path.EndsWith("/git-upload-pack") then
                        use body = new MemoryStream()
                        do! context.Request.InputStream.CopyToAsync(body)
                        let request = body.ToArray()
                        let response = SmartHttp.uploadPack source request |> value
                        requests.Add request
                        responseSizes.Add response.Length
                        context.Response.ContentType <- "application/x-git-upload-pack-result"
                        context.Response.ContentLength64 <- int64 response.Length
                        do! context.Response.OutputStream.WriteAsync(response)
                    else
                        context.Response.StatusCode <- 404
                    context.Response.Close()
            with
            | :? OperationCanceledException -> ()
            | :? HttpListenerException when cancellation.IsCancellationRequested -> ()
        }

        let runGit (cwd: string) (args: string) =
            use gitProcess = new Process()
            gitProcess.StartInfo <-
                ProcessStartInfo(
                    "git",
                    args,
                    WorkingDirectory = cwd,
                    UseShellExecute = false,
                    RedirectStandardOutput = true,
                    RedirectStandardError = true)
            gitProcess.StartInfo.Environment["GIT_TERMINAL_PROMPT"] <- "0"
            gitProcess.Start() |> ignore
            let stdout = gitProcess.StandardOutput.ReadToEnd()
            let stderr = gitProcess.StandardError.ReadToEnd()
            gitProcess.WaitForExit()
            if gitProcess.ExitCode <> 0 then
                failwithf "git %s failed (%d): %s%s" args gitProcess.ExitCode stdout stderr

        let client = Path.Combine(Path.GetDirectoryName source.GitDir, "client")
        let remote = $"http://127.0.0.1:{port}/repo.git"
        runGit (Path.GetDirectoryName client) $"clone --no-tags {remote} {client}"
        requests.Clear()
        responseSizes.Clear()

        let smallBlob = ObjectWriter.writeBlob source (Encoding.UTF8.GetBytes "incremental") |> value
        let updatedTree =
            ObjectWriter.writeTree source
                [| { Mode = 0o100644; Path = "large.bin"; Hash = largeBlob }
                   { Mode = 0o100644; Path = "small.txt"; Hash = smallBlob } |]
            |> value
        let updatedCommit = writeCommit source updatedTree [| initialCommit |] "updated"
        References.writeDirectAtomic source "refs/heads/master" updatedCommit |> value
        runGit client $"fetch --no-tags origin {updatedCommit}"
        let requestText =
            requests
            |> Seq.map Encoding.UTF8.GetString
            |> String.concat "\n"
        check (requestText.Contains($"have {initialCommit}")) "real git did not advertise its existing commit"
        let finalRequest =
            requests
            |> Seq.map Encoding.UTF8.GetString
            |> Seq.find (fun request -> request.Contains("done"))
        check
            (finalRequest.Contains($"have {initialCommit}"))
            "real git final done round did not repeat its common have"
        let bytes = responseSizes |> Seq.sum
        check
            (bytes < 512 * 1024)
            $"incremental fetch returned {bytes} bytes for one small commit"
        runGit client $"cat-file -e {updatedCommit}^{{commit}}"

        cancellation.Cancel()
        listener.Stop()
        server.GetAwaiter().GetResult())
[<EntryPoint>]
let main _ =
    acceptsPushWithoutWalkingHistoricalGraph ()
    rejectsMissingIntroducedGraphObject ()
    streamsPackOnlyAfterClientSendsDone ()
    realGitIncrementalFetchStaysIncremental ()
    printfn "upload-pack/receive-pack protocol contract passed"
    0
