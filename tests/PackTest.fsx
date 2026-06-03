// F3 conformance: delta encoder round-trip, reading git's own gc pack, and
// fsgit gc/repack producing a pack that `git fsck` and `git clone` accept.
//   dotnet build src/FSharpGit/FSharpGit.fsproj -c Debug && dotnet fsi tests/PackTest.fsx
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

let tmp = Path.Combine(Path.GetTempPath(), "fsgit-pack-" + Guid.NewGuid().ToString("N").Substring(0, 8))
Directory.CreateDirectory tmp |> ignore

// ---- A. delta encoder round-trips through PackData.applyDelta ----
let rt name (b: byte[]) (t: byte[]) =
    let d = Gc.encodeDelta b t
    check name (PackData.applyDelta b d = t)
let rep (s: string) n = Array.concat (Array.replicate n (Encoding.UTF8.GetBytes s))
rt "delta: identical" (rep "abcdefghijklmnop" 50) (rep "abcdefghijklmnop" 50)
rt "delta: append" (rep "abcdefghijklmnop" 50) (Array.append (rep "abcdefghijklmnop" 50) (Encoding.UTF8.GetBytes "tail"))
rt "delta: truncate" (rep "abcdefghijklmnop" 50) (rep "abcdefghijklmnop" 20)
rt "delta: insert middle" (Encoding.UTF8.GetBytes (String.replicate 40 "0123456789"))
                          (Encoding.UTF8.GetBytes (String.replicate 20 "0123456789" + "ZZZ" + String.replicate 20 "0123456789"))
rt "delta: unrelated" (Encoding.UTF8.GetBytes "hello world this is base") (Encoding.UTF8.GetBytes "completely different content here")

// ---- B. fsgit reads objects from git's OWN (delta) packfile ----
let bwork = Path.Combine(tmp, "bwork")
Directory.CreateDirectory bwork |> ignore
git bwork ["init"; "-q"; "-b"; "main"] |> ignore
git bwork ["config"; "user.email"; "t@t"] |> ignore
git bwork ["config"; "user.name"; "t"] |> ignore
for i in 1..5 do
    File.WriteAllText(Path.Combine(bwork, "doc.txt"), String.replicate i "line of content\n")
    git bwork ["add"; "-A"] |> ignore
    git bwork ["commit"; "-q"; "-m"; sprintf "c%d" i] |> ignore
let blobHash = (snd (git bwork ["rev-parse"; "HEAD:doc.txt"])).Trim()
let headHash = (snd (git bwork ["rev-parse"; "HEAD"])).Trim()
git bwork ["repack"; "-ad"] |> ignore
git bwork ["prune-packed"] |> ignore
let repoB = match Repository.openRepo bwork with Ok r -> r | Error e -> failwith e
check "fsgit reads blob from git's pack"
    (match ReadObjects.readBlob repoB blobHash with Ok b -> b = Encoding.UTF8.GetBytes(String.replicate 5 "line of content\n") | _ -> false)
check "fsgit reads commit from git's pack" (match ReadObjects.readCommit repoB headHash with Ok _ -> true | _ -> false)

// ---- C/D. fsgit gc/repack, then git fsck + clone (delta wire) ----
let bare = Path.Combine(tmp, "srv.git")
let repo = match Repository.initBare bare with Ok r -> r | Error e -> failwith e
let port = 5097
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

// push the 5-commit history
let pwork = Path.Combine(tmp, "pwork")
git tmp ["clone"; "-q"; bwork; pwork] |> ignore   // reuse bwork history
git pwork ["push"; remote; "main"] |> ignore

let looseBefore = Directory.GetDirectories(Path.Combine(bare, "objects")) |> Array.filter (fun d -> (Path.GetFileName d).Length = 2) |> Array.length
let n = match Gc.repack repo with Ok n -> n | Error e -> failwithf "repack: %s" e
check "repack packed objects" (n > 0)
let looseAfter = Directory.GetDirectories(Path.Combine(bare, "objects")) |> Array.filter (fun d -> (Path.GetFileName d).Length = 2) |> Array.length
check "loose objects removed after gc" (looseBefore > 0 && looseAfter = 0)
check "pack file present after gc" (Directory.GetFiles(Path.Combine(bare, "objects", "pack"), "*.pack").Length = 1)

// git fsck validates our pack + idx (crc, offsets, deltas)
let fsckCode, fsckOut = git tmp ["--git-dir"; bare; "fsck"; "--full"]
check "git fsck clean on fsgit-repacked repo" (fsckCode = 0)
if fsckCode <> 0 then printfn "    fsck: %s" (fsckOut.Trim())

// fsgit serves a clone FROM the delta pack (PackStore read + delta wire pack)
let cl = Path.Combine(tmp, "cl")
let clCode, clOut = git tmp ["clone"; "-q"; remote; cl]
check "git clone from gc'd repo succeeds" (clCode = 0)
let clFsck, _ = git cl ["fsck"; "--full"]
check "clone from gc'd repo is fsck-clean" (clFsck = 0)
check "cloned content matches"
    (File.Exists (Path.Combine(cl, "doc.txt")) && File.ReadAllText(Path.Combine(cl, "doc.txt")) = String.replicate 5 "line of content\n")

listener.Stop()
try Directory.Delete(tmp, true) with _ -> ()
printfn ""
if failures = 0 then printfn "ALL F3 (PACK/GC) CHECKS PASSED" else (printfn "%d FAILED" failures; exit 1)
