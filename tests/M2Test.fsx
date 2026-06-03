// M2 conformance test: real `git push` and `git clone` against fsgit's smart-HTTP
// server, served over HttpListener. Run inside the dotnet SDK image:
//   dotnet build src/FSharpGit/FSharpGit.fsproj -c Debug
//   dotnet fsi tests/M2Test.fsx
#I __SOURCE_DIRECTORY__
#r "../src/FSharpGit/bin/Debug/net10.0/FSharpGit.dll"

open System
open System.IO
open System.IO.Compression
open System.Net
open System.Diagnostics
open System.Text
open System.Threading
open FSharpGit

let mutable failures = 0
let check name cond =
    if cond then printfn "  ok   - %s" name
    else (failures <- failures + 1; printfn "  FAIL - %s" name)

let run (workDir: string) (cmd: string) (args: string list) : int * string =
    let psi = ProcessStartInfo(cmd, RedirectStandardOutput = true, RedirectStandardError = true,
                               UseShellExecute = false, WorkingDirectory = workDir)
    for a in args do psi.ArgumentList.Add a
    psi.Environment.["GIT_TERMINAL_PROMPT"] <- "0"
    use p = Process.Start psi
    let out = p.StandardOutput.ReadToEnd()
    let err = p.StandardError.ReadToEnd()
    p.WaitForExit()
    p.ExitCode, (out + err)

let git workDir args =
    let code, o = run workDir "git" args
    if code <> 0 then printfn "    (git %s) exit %d: %s" (String.Join(" ", args)) code (o.Trim())
    code, o

let tmp = Path.Combine(Path.GetTempPath(), "fsgit-m2-" + Guid.NewGuid().ToString("N").Substring(0, 8))
Directory.CreateDirectory tmp |> ignore
printfn "workdir: %s" tmp

// ---- bare server repo ----
let bare = Path.Combine(tmp, "srv.git")
let repo = match Repository.initBare bare with Ok r -> r | Error e -> failwithf "initBare: %s" e

// ---- HTTP server hosting the 3 smart-HTTP endpoints ----
let port = 5071
let prefix = sprintf "http://127.0.0.1:%d/" port
let listener = new HttpListener()
listener.Prefixes.Add prefix
listener.Start()

let handle (ctx: HttpListenerContext) =
    try
        let req = ctx.Request
        let path = req.Url.AbsolutePath
        let readBody () =
            use ms = new MemoryStream()
            req.InputStream.CopyTo ms
            let raw = ms.ToArray()
            if req.Headers.["Content-Encoding"] = "gzip" then
                use gs = new GZipStream(new MemoryStream(raw), CompressionMode.Decompress)
                use o = new MemoryStream()
                gs.CopyTo o
                o.ToArray()
            else raw
        let respond (ctype: string) (body: byte[]) =
            ctx.Response.ContentType <- ctype
            ctx.Response.StatusCode <- 200
            ctx.Response.OutputStream.Write(body, 0, body.Length)
            ctx.Response.OutputStream.Close()
        if path.EndsWith "/info/refs" then
            let svc = req.QueryString.["service"]
            respond (sprintf "application/x-%s-advertisement" svc) (SmartHttp.advertiseRefs repo svc)
        elif path.EndsWith "/git-upload-pack" then
            match SmartHttp.uploadPack repo (readBody ()) with
            | Ok b -> respond "application/x-git-upload-pack-result" b
            | Error e -> ctx.Response.StatusCode <- 500; ctx.Response.OutputStream.Close(); printfn "    upload-pack error: %s" e
        elif path.EndsWith "/git-receive-pack" then
            respond "application/x-git-receive-pack-result" (SmartHttp.receivePack repo (readBody ()))
        else
            ctx.Response.StatusCode <- 404
            ctx.Response.OutputStream.Close()
    with ex -> printfn "    handler exception: %s" ex.Message

let serverThread = Thread(fun () ->
    while listener.IsListening do
        try handle (listener.GetContext()) with _ -> ())
serverThread.IsBackground <- true
serverThread.Start()

let remote = sprintf "%ssrv.git" prefix

// ---- 1. real git creates content and PUSHES to fsgit ----
let work = Path.Combine(tmp, "work")
Directory.CreateDirectory work |> ignore
git work ["init"; "-q"; "-b"; "main"] |> ignore
git work ["config"; "user.email"; "t@t"] |> ignore
git work ["config"; "user.name"; "t"] |> ignore
Directory.CreateDirectory (Path.Combine(work, "src")) |> ignore
File.WriteAllText(Path.Combine(work, "README.md"), "# fsgit forge\nhello\n")
File.WriteAllText(Path.Combine(work, "src", "main.fs"), "let main () = 42\n")
git work ["add"; "-A"] |> ignore
git work ["commit"; "-q"; "-m"; "initial commit"] |> ignore
let localHead = (snd (git work ["rev-parse"; "HEAD"])).Trim()
let pushCode, pushOut = git work ["push"; remote; "main"]
check "git push succeeds" (pushCode = 0)

// read a server ref file directly
let serverRef name =
    let p = Path.Combine(bare, name)
    if File.Exists p then (File.ReadAllText p).Trim() else "?"
let serverMain = serverRef "refs/heads/main"
check "server refs/heads/main == pushed HEAD" (serverMain = localHead)

// ---- 2. real git CLONES from fsgit and gets identical content ----
let clone1 = Path.Combine(tmp, "clone1")
let cloneCode, cloneOut = git tmp ["clone"; "-q"; remote; clone1]
check "git clone succeeds" (cloneCode = 0)
check "cloned HEAD == original" ((snd (git clone1 ["rev-parse"; "HEAD"])).Trim() = localHead)
check "cloned README matches" (File.Exists (Path.Combine(clone1, "README.md")) && File.ReadAllText(Path.Combine(clone1, "README.md")) = "# fsgit forge\nhello\n")
check "cloned nested file matches" (File.Exists (Path.Combine(clone1, "src", "main.fs")))
let fsckCode, _ = git clone1 ["fsck"; "--full"]
check "git fsck on clone is clean" (fsckCode = 0)

// ---- 3. second push (incremental history) then fresh clone ----
File.WriteAllText(Path.Combine(work, "README.md"), "# fsgit forge\nhello\nmore\n")
File.WriteAllText(Path.Combine(work, "CHANGES.txt"), "v2\n")
git work ["add"; "-A"] |> ignore
git work ["commit"; "-q"; "-m"; "second commit"] |> ignore
git work ["checkout"; "-q"; "-b"; "feature"] |> ignore
File.WriteAllText(Path.Combine(work, "feature.txt"), "branchwork\n")
git work ["add"; "-A"] |> ignore
git work ["commit"; "-q"; "-m"; "feature work"] |> ignore
let push2, _ = git work ["push"; remote; "main"]
let push3, _ = git work ["push"; remote; "feature"]
check "second push (main) succeeds" (push2 = 0)
check "feature branch push succeeds" (push3 = 0)
let mainHead = (snd (git work ["rev-parse"; "main"])).Trim()
let featHead = (snd (git work ["rev-parse"; "feature"])).Trim()

let clone2 = Path.Combine(tmp, "clone2")
let clone2Code, _ = git tmp ["clone"; "-q"; remote; clone2]
check "second clone succeeds" (clone2Code = 0)
check "clone2 main == pushed" ((snd (git clone2 ["rev-parse"; "origin/main"])).Trim() = mainHead)
check "clone2 feature branch present" ((snd (git clone2 ["rev-parse"; "origin/feature"])).Trim() = featHead)
let fsck2, _ = git clone2 ["fsck"; "--full"]
check "git fsck on clone2 clean" (fsck2 = 0)
let logCount = (snd (git clone2 ["rev-list"; "--count"; "origin/main"])).Trim()
check "clone2 main has 2 commits" (logCount = "2")

listener.Stop()
try Directory.Delete(tmp, true) with _ -> ()
printfn ""
if failures = 0 then printfn "ALL M2 CHECKS PASSED" else (printfn "%d CHECK(S) FAILED" failures; exit 1)
