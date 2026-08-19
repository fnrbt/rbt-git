open System
open System.IO
open System.Text
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

[<EntryPoint>]
let main _ =
    acceptsPushWithoutWalkingHistoricalGraph ()
    rejectsMissingIntroducedGraphObject ()
    printfn "receive-pack bounded verification contract passed"
    0
