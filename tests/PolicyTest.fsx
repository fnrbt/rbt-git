// F1 conformance: push safety policy (CAS, non-FF rejection on protected refs,
// force-push allowed on feature branches), driven by real `git push`.
//   dotnet build src/FSharpGit/FSharpGit.fsproj -c Debug && dotnet fsi tests/PolicyTest.fsx
#I __SOURCE_DIRECTORY__
#r "../src/FSharpGit/bin/Debug/net10.0/FSharpGit.dll"

open System
open System.IO
open System.Net
open System.Diagnostics
open System.Text
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
let writeCommit (wd: string) (file: string) (content: string) (msg: string) =
    File.WriteAllText(Path.Combine(wd, file), content)
    git wd ["add"; "-A"] |> ignore
    git wd ["commit"; "-q"; "-m"; msg] |> ignore

let tmp = Path.Combine(Path.GetTempPath(), "fsgit-pol-" + Guid.NewGuid().ToString("N").Substring(0, 8))
Directory.CreateDirectory tmp |> ignore
let bare = Path.Combine(tmp, "srv.git")
let repo = match Repository.initBare bare with Ok r -> r | Error e -> failwithf "initBare: %s" e
// main is protected; feature branches may be force-pushed
let opts : SmartHttp.ReceiveOptions = { ProtectedRefs = [ "refs/heads/main" ]; AllowNonFastForward = true }

let port = 5083
let prefix = sprintf "http://127.0.0.1:%d/" port
let listener = new HttpListener()
listener.Prefixes.Add prefix
listener.Start()
let handle (ctx: HttpListenerContext) =
    try
        let req = ctx.Request
        let path = req.Url.AbsolutePath
        let body () = use ms = new MemoryStream() in req.InputStream.CopyTo ms; ms.ToArray()
        let respond (ct: string) (b: byte[]) = ctx.Response.ContentType <- ct; ctx.Response.OutputStream.Write(b, 0, b.Length); ctx.Response.OutputStream.Close()
        if path.EndsWith "/info/refs" then
            let svc = req.QueryString.["service"]
            respond (sprintf "application/x-%s-advertisement" svc) (SmartHttp.advertiseRefs repo svc)
        elif path.EndsWith "/git-upload-pack" then
            match SmartHttp.uploadPack repo (body ()) with
            | Ok b -> respond "application/x-git-upload-pack-result" b | Error _ -> ctx.Response.StatusCode <- 500; ctx.Response.OutputStream.Close()
        elif path.EndsWith "/git-receive-pack" then
            respond "application/x-git-receive-pack-result" (SmartHttp.receivePackWith repo opts (body ()))
        else ctx.Response.StatusCode <- 404; ctx.Response.OutputStream.Close()
    with ex -> printfn "    handler ex: %s" ex.Message
let t = Thread(fun () -> while listener.IsListening do (try handle (listener.GetContext()) with _ -> ()))
t.IsBackground <- true
t.Start()
let remote = sprintf "%ssrv.git" prefix

// client A
let a = Path.Combine(tmp, "a")
Directory.CreateDirectory a |> ignore
git a ["init"; "-q"; "-b"; "main"] |> ignore
git a ["config"; "user.email"; "a@a"] |> ignore
git a ["config"; "user.name"; "a"] |> ignore
writeCommit a "f.txt" "one\n" "c1"
let c1push, _ = git a ["push"; remote; "main"]
check "create main push ok" (c1push = 0)

writeCommit a "f.txt" "two\n" "c2"
let c2push, _ = git a ["push"; remote; "main"]
check "fast-forward main push ok" (c2push = 0)

// client B clones at c2, then A advances main, then B (stale) pushes -> CAS reject
let b = Path.Combine(tmp, "b")
git tmp ["clone"; "-q"; remote; b] |> ignore
git b ["config"; "user.email"; "b@b"] |> ignore
git b ["config"; "user.name"; "b"] |> ignore
writeCommit a "f.txt" "three\n" "c3"
git a ["push"; remote; "main"] |> ignore   // server main = c3
writeCommit b "g.txt" "bee\n" "bc"
let bpush, bout = git b ["push"; remote; "main"]
check "stale push rejected (CAS / fetch first)" (bpush <> 0 && (bout.Contains "fetch first" || bout.Contains "rejected"))

// force-push (non-FF) on PROTECTED main -> rejected
// A rewrites main: reset to c1 and commit an alternate
let c1 = (snd (git a ["rev-list"; "--max-parents=0"; "HEAD"])).Trim()
git a ["reset"; "--hard"; c1] |> ignore
writeCommit a "f.txt" "alt\n" "c2-alt"
let fpMain, fpMainOut = git a ["push"; "--force"; remote; "main"]
check "force-push on protected main rejected" (fpMain <> 0 && fpMainOut.Contains "non-fast-forward")

// force-push (non-FF) on a FEATURE branch -> allowed
git a ["checkout"; "-q"; "-b"; "feature"] |> ignore
writeCommit a "h.txt" "feat1\n" "feat1"
let featCreate, _ = git a ["push"; remote; "feature"]
check "feature create push ok" (featCreate = 0)
git a ["reset"; "--hard"; "HEAD~1"] |> ignore
writeCommit a "h.txt" "feat1-rebased\n" "feat1-rebased"
let featForce, featForceOut = git a ["push"; "--force"; remote; "feature"]
check "force-push on feature branch allowed" (featForce = 0)

listener.Stop()
try Directory.Delete(tmp, true) with _ -> ()
printfn ""
if failures = 0 then printfn "ALL F1 (POLICY) CHECKS PASSED" else (printfn "%d FAILED" failures; exit 1)
