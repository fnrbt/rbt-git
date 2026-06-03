// True-streaming conformance: serve clones by streaming uploadPackTo directly to
// the (non-seekable) HTTP response stream — the same path the RedBat route uses —
// on a larger repo. Proves the streaming writer needs no seeking/buffering and
// interoperates with real git.
//   dotnet build src/FSharpGit/FSharpGit.fsproj -c Debug && dotnet fsi tests/StreamTest.fsx
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
    let o = p.StandardOutput.ReadToEnd()
    let e = p.StandardError.ReadToEnd()
    p.WaitForExit()
    p.ExitCode, (o + e)
let git wd args = run wd "git" args

let tmp = Path.Combine(Path.GetTempPath(), "fsgit-stream-" + Guid.NewGuid().ToString("N").Substring(0, 8))
Directory.CreateDirectory tmp |> ignore
let bare = Path.Combine(tmp, "srv.git")
let repo = match Repository.initBare bare with Ok r -> r | Error e -> failwith e

let port = 5123
let prefix = sprintf "http://127.0.0.1:%d/" port
let listener = new HttpListener()
listener.Prefixes.Add prefix
listener.Start()
let handle (ctx: HttpListenerContext) =
    try
        let req = ctx.Request
        let path = req.Url.AbsolutePath
        let body () = use ms = new MemoryStream() in req.InputStream.CopyTo ms; ms.ToArray()
        if path.EndsWith "/info/refs" then
            let svc = req.QueryString.["service"]
            ctx.Response.ContentType <- sprintf "application/x-%s-advertisement" svc
            let b = SmartHttp.advertiseRefs repo svc
            ctx.Response.OutputStream.Write(b, 0, b.Length); ctx.Response.OutputStream.Close()
        elif path.EndsWith "/git-upload-pack" then
            ctx.Response.ContentType <- "application/x-git-upload-pack-result"
            // STREAM directly to the non-seekable network stream (no buffering).
            match SmartHttp.uploadPackTo repo (body ()) ctx.Response.OutputStream with
            | Ok () -> () | Error e -> printfn "    upload err: %s" e
            ctx.Response.OutputStream.Close()
        elif path.EndsWith "/git-receive-pack" then
            ctx.Response.ContentType <- "application/x-git-receive-pack-result"
            let b = SmartHttp.receivePack repo (body ())
            ctx.Response.OutputStream.Write(b, 0, b.Length); ctx.Response.OutputStream.Close()
        else ctx.Response.StatusCode <- 404; ctx.Response.OutputStream.Close()
    with ex -> printfn "    handler ex: %s" ex.Message
let th = Thread(fun () -> while listener.IsListening do (try handle (listener.GetContext()) with _ -> ()))
th.IsBackground <- true
th.Start()
let remote = sprintf "%ssrv.git" prefix

// larger repo: 30 files, 15 commits (each commit rewrites several files -> many blob versions)
let work = Path.Combine(tmp, "work")
Directory.CreateDirectory work |> ignore
git work ["init"; "-q"; "-b"; "main"] |> ignore
git work ["config"; "user.email"; "t@t"] |> ignore
git work ["config"; "user.name"; "t"] |> ignore
let rnd = Random(42)
for c in 1..15 do
    for f in 1..30 do
        if c = 1 || f % (1 + (c % 4)) = 0 then
            let lines = [ for i in 1 .. 40 -> sprintf "file %d commit %d line %d value %d\n" f c i (rnd.Next 1000) ]
            File.WriteAllText(Path.Combine(work, sprintf "f%02d.txt" f), String.Concat lines)
    git work ["add"; "-A"] |> ignore
    git work ["commit"; "-q"; "-m"; sprintf "commit %d" c] |> ignore
git work ["push"; remote; "main"] |> ignore
let headHash = (snd (git work ["rev-parse"; "HEAD"])).Trim()
let objCount = (snd (git work ["rev-list"; "--objects"; "HEAD"])).Trim().Split('\n').Length
printfn "  repo: %d objects, head %s" objCount (headHash.Substring(0, 8))

// full clone streamed to non-seekable response
let cl = Path.Combine(tmp, "cl")
let c1, o1 = git tmp ["clone"; "-q"; remote; cl]
check "streamed full clone succeeds" (c1 = 0)
if c1 <> 0 then printfn "    %s" (o1.Trim())
check "streamed clone HEAD matches" ((snd (git cl ["rev-parse"; "HEAD"])).Trim() = headHash)
let f1, _ = git cl ["fsck"; "--full"]
check "streamed clone fsck clean" (f1 = 0)
check "streamed clone object count matches" ((snd (git cl ["rev-list"; "--objects"; "HEAD"])).Trim().Split('\n').Length = objCount)
// spot-check a file's content equality across all 30 files
let mutable allMatch = true
for f in 1..30 do
    let p = Path.Combine(work, sprintf "f%02d.txt" f)
    let q = Path.Combine(cl, sprintf "f%02d.txt" f)
    if File.Exists p then allMatch <- allMatch && File.Exists q && File.ReadAllBytes p = File.ReadAllBytes q
check "streamed clone: all 30 files byte-identical" allMatch

// shallow clone over the streaming path too
let sh = Path.Combine(tmp, "sh")
let c2, _ = git tmp ["clone"; "-q"; "--depth"; "1"; remote; sh]
check "streamed shallow clone succeeds" (c2 = 0)
check "streamed shallow has 1 commit" ((snd (git sh ["rev-list"; "--count"; "HEAD"])).Trim() = "1")

listener.Stop()
try Directory.Delete(tmp, true) with _ -> ()
printfn ""
if failures = 0 then printfn "ALL STREAMING CHECKS PASSED" else (printfn "%d FAILED" failures; exit 1)
