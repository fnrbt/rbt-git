// Thorough end-to-end test against a real, large source corpus built from the
// redbat-forge repo (/work/.gitcorpus): exercises object read/write of every
// type, multi-pack + delta reads, history walk with merges, diff, 3-way merge,
// tags, serve (v0/v2/shallow), streaming push, and gc — all validated by real
// git. Each git call is killed after 150s so nothing can hang.
//   dotnet build src/FSharpGit/FSharpGit.fsproj -c Debug && dotnet fsi tests/BigRepoTest.fsx
#I __SOURCE_DIRECTORY__
#r "../src/FSharpGit/bin/Debug/net10.0/FSharpGit.dll"

open System
open System.IO
open System.Net
open System.Diagnostics
open System.Threading
open FSharpGit

let mutable failures = 0
let check name cond = if cond then printfn "  ok   - %s" name else (failures <- failures + 1; printfn "  FAIL - %s" name)
let section s = printfn "\n== %s ==" s
let run (wd: string) (cmd: string) (args: string list) : int * string =
    let psi = ProcessStartInfo(cmd, RedirectStandardOutput = true, RedirectStandardError = true, UseShellExecute = false, WorkingDirectory = wd)
    for a in args do psi.ArgumentList.Add a
    psi.Environment.["GIT_TERMINAL_PROMPT"] <- "0"
    use p = Process.Start psi
    let outT = p.StandardOutput.ReadToEndAsync()
    let errT = p.StandardError.ReadToEndAsync()
    let exited = p.WaitForExit 150000
    if not exited then (try p.Kill true with _ -> ())
    p.WaitForExit 3000 |> ignore
    (if exited then p.ExitCode else -999), ((try outT.Result with _ -> "") + (try errT.Result with _ -> ""))
let git wd args = run wd "git" args
let sw () = Stopwatch.StartNew()

let src = "/work/.gitcorpus"
if not (Directory.Exists src) then failwithf "corpus not found at %s" src
let corpus = match Repository.openRepo src with Ok r -> r | Error e -> failwithf "openRepo: %s" e

let tmp = Path.Combine(Path.GetTempPath(), "fsgit-big-" + Guid.NewGuid().ToString("N").Substring(0, 8))
Directory.CreateDirectory tmp |> ignore

let mainHead = (snd (git src ["rev-parse"; "main"])).Trim()
let featHead = (snd (git src ["rev-parse"; "feature"])).Trim()
let v1Commit = (snd (git src ["rev-parse"; "v1^{commit}"])).Trim()
let v1Tag = (snd (git src ["rev-parse"; "v1"])).Trim()           // annotated tag object
let mainCount = (snd (git src ["rev-list"; "--count"; "main"])).Trim()
printfn "corpus: main=%s feat=%s tagObj=%s commits=%s" (mainHead.Substring(0,8)) (featHead.Substring(0,8)) (v1Tag.Substring(0,8)) mainCount

// ============================================================================
section "A. Reads on real objects (multi-pack + delta resolution)"
// ============================================================================
let walkCount = FSharpGit.CommitHistory.walkCommits corpus mainHead |> Seq.length
check "walkCommits count == git rev-list (history walk over merges)" (string walkCount = mainCount)
check "readCommit(main) ok" (match ReadObjects.readCommit corpus mainHead with Ok _ -> true | _ -> false)
let mainTree = match ReadObjects.readCommit corpus mainHead with Ok c -> c.Tree | _ -> ""
let entries = match ReadObjects.readTree corpus mainTree with Ok e -> e | _ -> [||]
check "readTree(main^{tree}) returns a large tree" (entries.Length > 3)
// recursive listing touches deep trees + many blobs read from the pack(s)
let recRes = FSharpGit.TreeOperations.listTreeRecursive corpus mainTree
let recCount = match recRes with Ok xs -> xs.Length | _ -> -1
check "listTreeRecursive reads hundreds of objects from packs" (recCount > 100)
// pick a known file and compare its blob bytes to git
let someFile = (snd (git src ["ls-tree"; "-r"; "--name-only"; "main"])).Split('\n') |> Array.filter (fun s -> s.EndsWith ".fs") |> Array.head
let gitBlob = (let _, o = run src "git" ["cat-file"; "blob"; sprintf "main:%s" someFile] in System.Text.Encoding.UTF8.GetBytes "")  // placeholder, compare via hash below
let gitBlobHash = (snd (git src ["rev-parse"; sprintf "main:%s" someFile])).Trim()
check "readBlob of a real file matches git's object" (match ReadObjects.readBlob corpus gitBlobHash with Ok b -> ObjectWriter.hashRaw "blob" b = gitBlobHash | _ -> false)
// annotated tag read
check "readTag(v1) points at the import commit"
    (match ReadObjects.readTag corpus v1Tag with Ok t -> t.Object = v1Commit && t.ObjectType = "commit" | _ -> false)
// readRawObject preserves bytes for every object type (verify hash round-trips)
let rawOk h = match ReadObjects.readRawObject corpus h with Ok (t, c) -> ObjectWriter.hashRaw t c = h | _ -> false
check "raw read hash round-trips: commit/tree/blob/tag"
    (rawOk mainHead && rawOk mainTree && rawOk gitBlobHash && rawOk v1Tag)

// ============================================================================
section "B. Diff + merge-base + 3-way merge vs git"
// ============================================================================
let diffRes = FSharpGit.Diff.diffCommits corpus v1Commit mainHead
let fsgitChanged = match diffRes with Ok cs -> cs |> Array.map (fun c -> c.Path) |> Set.ofArray | _ -> Set.empty
let gitChanged = (snd (git src ["diff"; "--name-only"; v1Commit; mainHead])).Split('\n') |> Array.filter (fun s -> s <> "") |> Set.ofArray
check "diffCommits path set == git diff --name-only" (fsgitChanged = gitChanged)
// merge-base
let mb = (snd (git src ["merge-base"; "main"; "feature"])).Trim()
check "merge-base(main,feature) == git" (mb = v1Commit)
// 3-way merge: compare fsgit's verdict/tree to git merge-tree
let gtCode, gtOut = git src ["merge-tree"; "--write-tree"; "main"; "feature"]
let gitMergeTree = gtOut.Trim().Split('\n').[0]
let sig0 : Signature = { Name = "t"; Email = "t@t"; Time = DateTimeOffset.FromUnixTimeSeconds 1700000000L }
match FSharpGit.Merge3.merge corpus mainHead featHead sig0 "merge" with
| FSharpGit.Merge3.Merged mc ->
    let tr = match ReadObjects.readCommit corpus mc with Ok c -> c.Tree | _ -> "?"
    check "git reports a clean merge" (gtCode = 0)
    check "Merge3 tree == git merge-tree" (tr = gitMergeTree)
| FSharpGit.Merge3.Conflicts _ ->
    check "git also reports a conflict" (gtCode <> 0)
| FSharpGit.Merge3.MergeError e -> check "Merge3 no error" false; printfn "    %s" e

// ============================================================================
// HTTP server: serves the corpus (read) and a fresh bare repo (push target)
// ============================================================================
let bare = Path.Combine(tmp, "bare.git")
let bareRepo = match Repository.initBare bare with Ok r -> r | Error e -> failwith e
let port = 5151
let prefix = sprintf "http://127.0.0.1:%d/" port
let listener = new HttpListener()
listener.Prefixes.Add prefix
listener.Start()
let handle (ctx: HttpListenerContext) =
    try
        let req = ctx.Request
        let path = req.Url.AbsolutePath
        let repo = if path.Contains "/corpus.git/" then corpus else bareRepo
        let out = ctx.Response.OutputStream
        let isV2 = let h = req.Headers.["Git-Protocol"] in not (isNull h) && h.Contains "version=2"
        let body () = use ms = new MemoryStream() in req.InputStream.CopyTo ms; ms.ToArray()
        if path.EndsWith "/info/refs" then
            let svc = req.QueryString.["service"]
            ctx.Response.ContentType <- sprintf "application/x-%s-advertisement" svc
            let b = if isV2 && svc = "git-upload-pack" then SmartHttp.advertiseRefsV2 repo else SmartHttp.advertiseRefs repo svc
            out.Write(b, 0, b.Length); out.Close()
        elif path.EndsWith "/git-upload-pack" then
            ctx.Response.ContentType <- "application/x-git-upload-pack-result"
            (if isV2 then SmartHttp.uploadPackV2To repo (body ()) out else SmartHttp.uploadPackTo repo (body ()) out) |> ignore
            out.Close()
        elif path.EndsWith "/git-receive-pack" then
            ctx.Response.ContentType <- "application/x-git-receive-pack-result"
            SmartHttp.receivePackStreamWith repo SmartHttp.defaultReceiveOptions req.InputStream out
            out.Close()
        else ctx.Response.StatusCode <- 404; out.Close()
    with ex -> printfn "    handler ex: %s" ex.Message
let th = Thread(fun () -> while listener.IsListening do (try handle (listener.GetContext()) with _ -> ()))
th.IsBackground <- true
th.Start()
let corpusRemote = sprintf "%scorpus.git" prefix
let bareRemote = sprintf "%sbare.git" prefix

// reference: a direct git clone of the corpus
let direct = Path.Combine(tmp, "direct")
git tmp ["clone"; "-q"; src; direct] |> ignore
let dTree = (snd (git direct ["rev-parse"; "HEAD^{tree}"])).Trim()

// ============================================================================
section "C. Serve clones from fsgit (v0 / v2 / shallow) vs direct clone"
// ============================================================================
let cloneAndCheck label extra =
    let dir = Path.Combine(tmp, label)
    let t = sw ()
    let code, out = git tmp (extra @ ["clone"; "-q"; corpusRemote; dir])
    printfn "  (%s clone took %dms)" label t.ElapsedMilliseconds
    if code <> 0 then printfn "    %s" (out.Trim())
    check (sprintf "%s clone succeeds" label) (code = 0)
    if code = 0 then
        check (sprintf "%s HEAD matches" label) ((snd (git dir ["rev-parse"; "HEAD"])).Trim() = mainHead)
        check (sprintf "%s fsck clean" label) (fst (git dir ["fsck"; "--full"]) = 0)
    dir
let v0dir = cloneAndCheck "v0" ["-c"; "protocol.version=0"]
check "v0 clone HEAD tree == direct" ((snd (git v0dir ["rev-parse"; "HEAD^{tree}"])).Trim() = dTree)
check "v0 clone object count == direct" ((snd (git v0dir ["rev-list"; "--objects"; "--all"])).Split('\n').Length = (snd (git direct ["rev-list"; "--objects"; "--all"])).Split('\n').Length)
check "v0 clone fetched the annotated tag" ((snd (git v0dir ["tag"])).Trim().Contains "v1")
let v2dir = cloneAndCheck "v2" ["-c"; "protocol.version=2"]
check "v2 clone HEAD tree == direct" ((snd (git v2dir ["rev-parse"; "HEAD^{tree}"])).Trim() = dTree)
let shdir = Path.Combine(tmp, "shallow")
let shc, _ = git tmp ["-c"; "protocol.version=2"; "clone"; "-q"; "--depth"; "1"; corpusRemote; shdir]
check "v2 shallow clone succeeds" (shc = 0)
check "shallow clone has 1 commit" ((snd (git shdir ["rev-list"; "--count"; "HEAD"])).Trim() = "1")
check "shallow clone HEAD tree == direct (full working tree present)" ((snd (git shdir ["rev-parse"; "HEAD^{tree}"])).Trim() = dTree)

// ============================================================================
section "D. Streaming push of the whole corpus into a fresh fsgit bare"
// ============================================================================
git src ["remote"; "remove"; "fsgit"] |> ignore
git src ["remote"; "add"; "fsgit"; bareRemote] |> ignore
let tpush = sw ()
let pCode, pOut = git src ["push"; "-q"; "fsgit"; "main"; "feature"; "--tags"]
printfn "  (push of corpus took %dms)" tpush.ElapsedMilliseconds
check "streaming push of main+feature+tags succeeds" (pCode = 0)
if pCode <> 0 then printfn "    %s" (pOut.Trim())
check "git fsck clean on fsgit bare after push" (fst (git tmp ["--git-dir"; bare; "fsck"; "--full"]) = 0)
check "pushed main ref matches" ((File.ReadAllText(Path.Combine(bare, "refs", "heads", "main"))).Trim() = mainHead)
check "pushed annotated tag present" (File.Exists (Path.Combine(bare, "refs", "tags", "v1")))
// clone the pushed bare back and compare to direct
let backDir = Path.Combine(tmp, "back")
check "clone back from pushed bare succeeds" (fst (git tmp ["clone"; "-q"; bareRemote; backDir]) = 0)
check "clone-back HEAD tree == direct" ((snd (git backDir ["rev-parse"; "HEAD^{tree}"])).Trim() = dTree)
check "clone-back has the feature branch" ((snd (git backDir ["rev-parse"; "origin/feature"])).Trim() = featHead)

// ============================================================================
section "E. gc/repack the pushed bare, then serve from the delta pack"
// ============================================================================
let looseBefore = Directory.GetDirectories(Path.Combine(bare, "objects")) |> Array.filter (fun d -> (Path.GetFileName d).Length = 2) |> Array.length
let tgc = sw ()
let packed = match FSharpGit.Gc.repack bareRepo with Ok n -> n | Error e -> failwithf "gc: %s" e
printfn "  (gc packed %d objects in %dms)" packed tgc.ElapsedMilliseconds
check "gc packed the objects" (packed > 100)
check "gc removed loose objects" (looseBefore > 0 && (Directory.GetDirectories(Path.Combine(bare, "objects")) |> Array.filter (fun d -> (Path.GetFileName d).Length = 2) |> Array.length) = 0)
check "git fsck clean after gc (delta pack + idx valid)" (fst (git tmp ["--git-dir"; bare; "fsck"; "--full"]) = 0)
let gcDir = Path.Combine(tmp, "gc")
check "clone from gc'd bare succeeds" (fst (git tmp ["clone"; "-q"; bareRemote; gcDir]) = 0)
check "clone-from-gc HEAD tree == direct" ((snd (git gcDir ["rev-parse"; "HEAD^{tree}"])).Trim() = dTree)
check "clone-from-gc fsck clean" (fst (git gcDir ["fsck"; "--full"]) = 0)

// ============================================================================
section "F. Object writing round-trips through real git (every type)"
// ============================================================================
let wbare = Path.Combine(tmp, "write.git")
let wrepo = match Repository.initBare wbare with Ok r -> r | Error e -> failwith e
let writeRound (label: string) (h: GitHash) =
    match ReadObjects.readRawObject corpus h with
    | Ok (t, c) ->
        match ObjectWriter.writeRaw wrepo t c with
        | Ok h2 ->
            let catType = (snd (run wbare "git" ["--git-dir"; wbare; "cat-file"; "-t"; h])).Trim()
            check (sprintf "%s: writeRaw hash preserved + git cat-file reads it" label) (h2 = h && catType = t)
        | Error e -> check (sprintf "%s write" label) false; printfn "    %s" e
    | Error e -> check (sprintf "%s read" label) false; printfn "    %s" e
writeRound "commit" mainHead
writeRound "tree" mainTree
writeRound "blob" gitBlobHash
writeRound "tag" v1Tag

listener.Stop()
try Directory.Delete(tmp, true) with _ -> ()
printfn ""
if failures = 0 then printfn "ALL BIG-REPO CHECKS PASSED" else (printfn "%d CHECK(S) FAILED" failures; exit 1)
