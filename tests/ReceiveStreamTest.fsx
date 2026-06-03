// Push-side streaming conformance: serve git-receive-pack by streaming
// receivePackStreamWith directly off the (non-seekable) request stream — the same
// path the RedBat route uses. Exercises ofs-delta (first push) and ref-delta /
// thin-pack (incremental push) resolution from disk, with real git.
//   dotnet build src/FSharpGit/FSharpGit.fsproj -c Debug && dotnet fsi tests/ReceiveStreamTest.fsx
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
let run (wd: string) (cmd: string) (args: string list) : int * string =
    let psi = ProcessStartInfo(cmd, RedirectStandardOutput = true, RedirectStandardError = true, UseShellExecute = false, WorkingDirectory = wd)
    for a in args do psi.ArgumentList.Add a
    psi.Environment.["GIT_TERMINAL_PROMPT"] <- "0"
    use p = Process.Start psi
    let outT = p.StandardOutput.ReadToEndAsync()
    let errT = p.StandardError.ReadToEndAsync()
    let exited = p.WaitForExit 25000
    if not exited then (try p.Kill true with _ -> ())
    p.WaitForExit 2000 |> ignore
    let out = try outT.Result with _ -> ""
    let err = try errT.Result with _ -> ""
    (if exited then p.ExitCode else -999), (out + err)
let git wd args = run wd "git" args

let tmp = Path.Combine(Path.GetTempPath(), "fsgit-rcv-" + Guid.NewGuid().ToString("N").Substring(0, 8))
Directory.CreateDirectory tmp |> ignore
let bare = Path.Combine(tmp, "srv.git")
let repo = match Repository.initBare bare with Ok r -> r | Error e -> failwith e

let port = 5141
let prefix = sprintf "http://127.0.0.1:%d/" port
let listener = new HttpListener()
listener.Prefixes.Add prefix
listener.Start()
let handle (ctx: HttpListenerContext) =
    try
        let req = ctx.Request
        let path = req.Url.AbsolutePath
        let out = ctx.Response.OutputStream
        if path.EndsWith "/info/refs" then
            let svc = req.QueryString.["service"]
            ctx.Response.ContentType <- sprintf "application/x-%s-advertisement" svc
            let b = SmartHttp.advertiseRefs repo svc
            out.Write(b, 0, b.Length); out.Close()
        elif path.EndsWith "/git-upload-pack" then
            ctx.Response.ContentType <- "application/x-git-upload-pack-result"
            use ms = new MemoryStream() in req.InputStream.CopyTo ms
            (match SmartHttp.uploadPackTo repo (ms.ToArray()) out with Ok () -> () | Error e -> printfn "    upload err: %s" e)
            out.Close()
        elif path.EndsWith "/git-receive-pack" then
            ctx.Response.ContentType <- "application/x-git-receive-pack-result"
            // STREAM the push directly off the non-seekable request stream.
            SmartHttp.receivePackStreamWith repo SmartHttp.defaultReceiveOptions req.InputStream out
            out.Close()
        else ctx.Response.StatusCode <- 404; out.Close()
    with ex -> printfn "    handler ex: %s" ex.Message
let th = Thread(fun () -> while listener.IsListening do (try handle (listener.GetContext()) with _ -> ()))
th.IsBackground <- true
th.Start()
let remote = sprintf "%ssrv.git" prefix

// build a repo with many similar file versions -> the push pack will use deltas
let work = Path.Combine(tmp, "work")
Directory.CreateDirectory work |> ignore
git work ["init"; "-q"; "-b"; "main"] |> ignore
git work ["config"; "user.email"; "t@t"] |> ignore
git work ["config"; "user.name"; "t"] |> ignore
let rnd = Random 7
for c in 1..10 do
    for f in 1..12 do
        let lines = [ for i in 1 .. 60 -> sprintf "file %d commit %d line %d %d\n" f c i (rnd.Next 100) ]
        File.WriteAllText(Path.Combine(work, sprintf "f%02d.txt" f), String.Concat lines)
    git work ["add"; "-A"] |> ignore
    git work ["commit"; "-q"; "-m"; sprintf "c%d" c] |> ignore
let head1 = (snd (git work ["rev-parse"; "HEAD"])).Trim()

// first push: streamed unpack of a delta pack off the network stream
let p1, p1o = git work ["push"; remote; "main"]
check "streamed push #1 succeeds" (p1 = 0)
if p1 <> 0 then printfn "    %s" (p1o.Trim())
check "server main == pushed head" ((File.ReadAllText(Path.Combine(bare, "refs", "heads", "main"))).Trim() = head1)
check "git fsck clean on server after streamed push" (fst (git tmp ["--git-dir"; bare; "fsck"; "--full"]) = 0)

// clone back and verify content
let cl = Path.Combine(tmp, "cl")
check "clone back after streamed push succeeds" (fst (git tmp ["clone"; "-q"; remote; cl]) = 0)
check "cloned head matches" ((snd (git cl ["rev-parse"; "HEAD"])).Trim() = head1)
let mutable allMatch = true
for f in 1..12 do
    let a = File.ReadAllBytes(Path.Combine(work, sprintf "f%02d.txt" f))
    let b = Path.Combine(cl, sprintf "f%02d.txt" f)
    allMatch <- allMatch && File.Exists b && File.ReadAllBytes b = a
check "all 12 files byte-identical after round trip" allMatch

// incremental push: thin pack delta'd against objects already on the server
for c in 11..13 do
    for f in 1..12 do
        let lines = [ for i in 1 .. 60 -> sprintf "file %d commit %d line %d %d\n" f c i (rnd.Next 100) ]
        File.WriteAllText(Path.Combine(work, sprintf "f%02d.txt" f), String.Concat lines)
    git work ["add"; "-A"] |> ignore
    git work ["commit"; "-q"; "-m"; sprintf "c%d" c] |> ignore
let head2 = (snd (git work ["rev-parse"; "HEAD"])).Trim()
let p2, p2o = git work ["push"; remote; "main"]
check "streamed incremental push (thin pack) succeeds" (p2 = 0)
if p2 <> 0 then printfn "    %s" (p2o.Trim())
check "git fsck clean after incremental push" (fst (git tmp ["--git-dir"; bare; "fsck"; "--full"]) = 0)

let cl2 = Path.Combine(tmp, "cl2")
check "clone after incremental push succeeds" (fst (git tmp ["clone"; "-q"; remote; cl2]) = 0)
check "incremental clone head matches" ((snd (git cl2 ["rev-parse"; "HEAD"])).Trim() = head2)
check "incremental clone has 13 commits" ((snd (git cl2 ["rev-list"; "--count"; "HEAD"])).Trim() = "13")

listener.Stop()
try Directory.Delete(tmp, true) with _ -> ()
printfn ""
if failures = 0 then printfn "ALL RECEIVE-STREAM CHECKS PASSED" else (printfn "%d FAILED" failures; exit 1)
