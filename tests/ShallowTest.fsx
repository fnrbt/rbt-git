// F4 conformance: shallow clone (depth negotiation) with real git.
//   dotnet build src/FSharpGit/FSharpGit.fsproj -c Debug && dotnet fsi tests/ShallowTest.fsx
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

let tmp = Path.Combine(Path.GetTempPath(), "fsgit-shallow-" + Guid.NewGuid().ToString("N").Substring(0, 8))
Directory.CreateDirectory tmp |> ignore
let bare = Path.Combine(tmp, "srv.git")
let repo = match Repository.initBare bare with Ok r -> r | Error e -> failwith e
let port = 5101
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
            match SmartHttp.uploadPack repo (body ()) with Ok b -> respond "application/x-git-upload-pack-result" b | Error _ -> ctx.Response.StatusCode <- 500; ctx.Response.OutputStream.Close()
        elif path.EndsWith "/git-receive-pack" then
            respond "application/x-git-receive-pack-result" (SmartHttp.receivePack repo (body ()))
        else ctx.Response.StatusCode <- 404; ctx.Response.OutputStream.Close()
    with ex -> printfn "    handler ex: %s" ex.Message
let th = Thread(fun () -> while listener.IsListening do (try handle (listener.GetContext()) with _ -> ()))
th.IsBackground <- true
th.Start()
let remote = sprintf "%ssrv.git" prefix

// 4-commit history
let work = Path.Combine(tmp, "work")
Directory.CreateDirectory work |> ignore
git work ["init"; "-q"; "-b"; "main"] |> ignore
git work ["config"; "user.email"; "t@t"] |> ignore
git work ["config"; "user.name"; "t"] |> ignore
for i in 1..4 do
    File.WriteAllText(Path.Combine(work, "f.txt"), sprintf "version %d\n" i)
    git work ["add"; "-A"] |> ignore
    git work ["commit"; "-q"; "-m"; sprintf "c%d" i] |> ignore
git work ["push"; remote; "main"] |> ignore

let cloneDepth d name =
    let dir = Path.Combine(tmp, name)
    let code, out = git tmp ["clone"; "-q"; "--depth"; string d; remote; dir]
    code, out, dir

let c1, o1, d1 = cloneDepth 1 "s1"
check "shallow clone --depth 1 succeeds" (c1 = 0)
if c1 <> 0 then printfn "    %s" (o1.Trim())
check "depth-1 clone has exactly 1 commit" ((snd (git d1 ["rev-list"; "--count"; "HEAD"])).Trim() = "1")
check "depth-1 clone has latest content" (File.Exists (Path.Combine(d1, "f.txt")) && File.ReadAllText(Path.Combine(d1, "f.txt")) = "version 4\n")
check "depth-1 clone is marked shallow" (File.Exists (Path.Combine(d1, ".git", "shallow")))

let c2, o2, d2 = cloneDepth 2 "s2"
check "shallow clone --depth 2 succeeds" (c2 = 0)
check "depth-2 clone has exactly 2 commits" ((snd (git d2 ["rev-list"; "--count"; "HEAD"])).Trim() = "2")

// full clone still works (now with ofs-delta wire advertised)
let df = Path.Combine(tmp, "full")
let cf, cfout = git tmp ["clone"; "-q"; remote; df]
check "full clone still works with delta wire" (cf = 0)
if cf <> 0 then printfn "    %s" (cfout.Trim())
check "full clone has all 4 commits" ((snd (git df ["rev-list"; "--count"; "HEAD"])).Trim() = "4")
let ff, _ = git df ["fsck"; "--full"]
check "full clone fsck clean" (ff = 0)

listener.Stop()
try Directory.Delete(tmp, true) with _ -> ()
printfn ""
if failures = 0 then printfn "ALL F4 (SHALLOW) CHECKS PASSED" else (printfn "%d FAILED" failures; exit 1)
