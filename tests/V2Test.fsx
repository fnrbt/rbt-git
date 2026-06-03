// Protocol v2 conformance with real git: v2 clone/shallow/fetch + v0 regression,
// through a dual harness that honors the Git-Protocol header. Every git call is
// killed after 20s so a protocol bug fails fast instead of hanging.
//   dotnet build src/FSharpGit/FSharpGit.fsproj -c Debug && dotnet fsi tests/V2Test.fsx
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
    let exited = p.WaitForExit 20000
    if not exited then (try p.Kill true with _ -> ())
    p.WaitForExit 2000 |> ignore
    let out = try outT.Result with _ -> ""
    let err = try errT.Result with _ -> ""
    (if exited then p.ExitCode else -999), (out + err)
let git wd args = run wd "git" args
let v2 = ["-c"; "protocol.version=2"]
let v0 = ["-c"; "protocol.version=0"]

let tmp = Path.Combine(Path.GetTempPath(), "fsgit-v2-" + Guid.NewGuid().ToString("N").Substring(0, 8))
Directory.CreateDirectory tmp |> ignore
let bare = Path.Combine(tmp, "srv.git")
let repo = match Repository.initBare bare with Ok r -> r | Error e -> failwith e

let port = 5131
let prefix = sprintf "http://127.0.0.1:%d/" port
let listener = new HttpListener()
listener.Prefixes.Add prefix
listener.Start()
let isV2req (req: HttpListenerRequest) =
    let h = req.Headers.["Git-Protocol"]
    not (isNull h) && h.Contains "version=2"
let handle (ctx: HttpListenerContext) =
    try
        let req = ctx.Request
        let path = req.Url.AbsolutePath
        let body () = use ms = new MemoryStream() in req.InputStream.CopyTo ms; ms.ToArray()
        let out = ctx.Response.OutputStream
        if path.EndsWith "/info/refs" then
            let svc = req.QueryString.["service"]
            ctx.Response.ContentType <- sprintf "application/x-%s-advertisement" svc
            let b =
                if isV2req req && svc = "git-upload-pack" then SmartHttp.advertiseRefsV2 repo
                else SmartHttp.advertiseRefs repo svc
            out.Write(b, 0, b.Length); out.Close()
        elif path.EndsWith "/git-upload-pack" then
            ctx.Response.ContentType <- "application/x-git-upload-pack-result"
            let r =
                if isV2req req then SmartHttp.uploadPackV2To repo (body ()) out
                else SmartHttp.uploadPackTo repo (body ()) out
            (match r with Ok () -> () | Error e -> printfn "    upload err: %s" e)
            out.Close()
        elif path.EndsWith "/git-receive-pack" then
            ctx.Response.ContentType <- "application/x-git-receive-pack-result"
            let b = SmartHttp.receivePack repo (body ())
            out.Write(b, 0, b.Length); out.Close()
        else ctx.Response.StatusCode <- 404; out.Close()
    with ex -> printfn "    handler ex: %s" ex.Message
let th = Thread(fun () -> while listener.IsListening do (try handle (listener.GetContext()) with _ -> ()))
th.IsBackground <- true
th.Start()
let remote = sprintf "%ssrv.git" prefix

// build + push a 3-commit history
let work = Path.Combine(tmp, "work")
Directory.CreateDirectory work |> ignore
git work ["init"; "-q"; "-b"; "main"] |> ignore
git work ["config"; "user.email"; "t@t"] |> ignore
git work ["config"; "user.name"; "t"] |> ignore
for i in 1..3 do
    File.WriteAllText(Path.Combine(work, "f.txt"), sprintf "v%d\n" i)
    File.WriteAllText(Path.Combine(work, sprintf "extra%d.txt" i), sprintf "extra %d\n" i)
    git work ["add"; "-A"] |> ignore
    git work ["commit"; "-q"; "-m"; sprintf "c%d" i] |> ignore
let pc, pout = git work (v2 @ ["push"; remote; "main"])   // push (falls back to v0 receive-pack)
check "push succeeds (v2 client -> v0 receive-pack)" (pc = 0)
let head3 = (snd (git work ["rev-parse"; "HEAD"])).Trim()

// 1. v2 full clone
let cl2 = Path.Combine(tmp, "cl2")
let c2, o2 = git tmp (v2 @ ["clone"; "-q"; remote; cl2])
check "v2 clone succeeds" (c2 = 0)
if c2 <> 0 then printfn "    %s" (o2.Trim())
check "v2 clone HEAD matches" ((snd (git cl2 ["rev-parse"; "HEAD"])).Trim() = head3)
check "v2 clone fsck clean" (fst (git cl2 ["fsck"; "--full"]) = 0)
check "v2 clone content matches" (File.Exists (Path.Combine(cl2, "f.txt")) && File.ReadAllText(Path.Combine(cl2, "f.txt")) = "v3\n")
check "v2 clone has 3 commits" ((snd (git cl2 ["rev-list"; "--count"; "HEAD"])).Trim() = "3")

// 2. v0 full clone (regression through same server)
let cl0 = Path.Combine(tmp, "cl0")
let c0, o0 = git tmp (v0 @ ["clone"; "-q"; remote; cl0])
check "v0 clone still works" (c0 = 0)
check "v0 clone HEAD matches" ((snd (git cl0 ["rev-parse"; "HEAD"])).Trim() = head3)

// 3. v2 shallow clone
let sh = Path.Combine(tmp, "sh")
let cs, oss = git tmp (v2 @ ["clone"; "-q"; "--depth"; "1"; remote; sh])
check "v2 shallow clone succeeds" (cs = 0)
if cs <> 0 then printfn "    %s" (oss.Trim())
check "v2 shallow has 1 commit" ((snd (git sh ["rev-list"; "--count"; "HEAD"])).Trim() = "1")
check "v2 shallow marked shallow" (File.Exists (Path.Combine(sh, ".git", "shallow")))

// 4. v2 incremental fetch: advance server, fetch into the v2 clone
File.WriteAllText(Path.Combine(work, "f.txt"), "v4\n")
git work ["add"; "-A"] |> ignore
git work ["commit"; "-q"; "-m"; "c4"] |> ignore
git work (v2 @ ["push"; remote; "main"]) |> ignore
let head4 = (snd (git work ["rev-parse"; "HEAD"])).Trim()
let fc, fout = git cl2 (v2 @ ["fetch"; "-q"; "origin"])
check "v2 incremental fetch succeeds" (fc = 0)
if fc <> 0 then printfn "    %s" (fout.Trim())
check "v2 fetch advanced origin/main" ((snd (git cl2 ["rev-parse"; "origin/main"])).Trim() = head4)
check "v2 fetch is fsck clean" (fst (git cl2 ["fsck"; "--full"]) = 0)

// 5. default clone (git 2.43 defaults to v2) exercises the v2 path end to end
let cd = Path.Combine(tmp, "cd")
check "default clone (v2 by default) works" (fst (git tmp ["clone"; "-q"; remote; cd]) = 0)

listener.Stop()
try Directory.Delete(tmp, true) with _ -> ()
printfn ""
if failures = 0 then printfn "ALL V2 CHECKS PASSED" else (printfn "%d FAILED" failures; exit 1)
