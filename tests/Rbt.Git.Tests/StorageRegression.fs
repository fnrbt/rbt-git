module StorageRegression

open System
open System.IO
open System.Text
open System.Diagnostics
open System.Threading.Tasks
open Rbt.Git

let private value = function Ok x -> x | Error e -> failwith e
let private check condition message = if not condition then failwith message
let private git dir (args: string list) =
    let info = ProcessStartInfo("git")
    info.WorkingDirectory <- dir
    info.RedirectStandardOutput <- true
    info.RedirectStandardError <- true
    for arg in args do info.ArgumentList.Add arg
    use gitProcess = Process.Start info
    let stdout = gitProcess.StandardOutput.ReadToEnd()
    let stderr = gitProcess.StandardError.ReadToEnd()
    gitProcess.WaitForExit()
    if gitProcess.ExitCode <> 0 then failwithf "git failed: %s" stderr
    stdout.Trim()

type private NoReadPastEnd(data: byte[]) =
    inherit MemoryStream(data)
    override this.Read(buffer, offset, count) =
        if count > 0 && this.Position = this.Length then failwith "read past trailer"
        base.Read(buffer, offset, count)
    override this.ReadByte() =
        if this.Position = this.Length then failwith "read past trailer"
        base.ReadByte()

let run () =
    let root = Path.Combine(Path.GetTempPath(), "rbt-storage-" + Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory root |> ignore
    try
        let repo = Repository.initBare (Path.Combine(root, "repo.git")) |> value
        let signature = { Name = "Test"; Email = "test@example.com"; Time = DateTimeOffset.FromUnixTimeSeconds(1L) }
        let content = Encoding.UTF8.GetBytes "same object"
        let writes = [| for _ in 1 .. 20 -> Task.Run(fun () -> ObjectWriter.writeBlob repo content |> value) |]
        Task.WaitAll(writes |> Array.map (fun t -> t :> Task))
        let blob = writes.[0].Result
        check (writes |> Array.forall (fun t -> t.Result = blob)) "concurrent loose writes disagreed"
        check (git repo.GitDir ["cat-file"; "-p"; blob] = "same object") "Git cannot read loose object"
        let tree = ObjectWriter.writeTree repo [| { Mode = 0o100644; Path = "file"; Hash = blob } |] |> value
        let commit tree parents = ObjectWriter.writeCommit repo { Tree = tree; Parents = parents; Author = signature; Committer = signature; Message = "test\n" } |> value
        let parent = commit tree [||]
        let tip = commit tree [| parent |]
        let tag target kind name = ObjectWriter.writeTag repo { Object = target; ObjectType = kind; Tag = name; Tagger = signature; Message = "tag\n" } |> value
        let inner = tag tip "commit" "inner"
        let outer = tag inner "tag" "outer"
        let closure = PackWriter.objectClosure repo [| outer |] [||] |> Set.ofArray
        check (Set.isSubset (Set.ofList [outer; inner; tip; parent; tree; blob]) closure) "nested tag closure is incomplete"
        let shallow, boundaries = PackWriter.objectClosureShallow repo [| outer |] 1
        check (Set.isSubset (Set.ofList [outer; inner; tip; tree; blob]) (Set.ofArray shallow)) "shallow tag closure is incomplete"
        check (boundaries = [| tip |]) "shallow boundary is incorrect"
        check (Set.isSubset (Set.ofList [tree; blob]) (PackWriter.objectClosure repo [| tree |] [||] |> Set.ofArray)) "tree tip closure is incomplete"
        let longRepo = Repository.initBare (Path.Combine(root, "long.git")) |> value
        let longTree = ObjectWriter.writeTree longRepo [||] |> value
        let writeLong parents =
            ObjectWriter.writeCommit longRepo { Tree = longTree; Parents = parents; Author = signature; Committer = signature; Message = "long\n" } |> value
        let first = writeLong [||]
        let mutable longTip = first
        for _ in 1 .. 1500 do longTip <- writeLong [| longTip |]
        let longClosure = PackWriter.objectClosure longRepo [| longTip |] [||] |> Set.ofArray
        check (longClosure.Contains first && longClosure.Contains longTip) "long commit history closure is incomplete"
        let missing = String.replicate 40 "1"
        use output = new MemoryStream()
        let mutable failed = false
        let badRepo = Repository.initBare (Path.Combine(root, "bad.git")) |> value
        let badBlob = ObjectWriter.writeBlob badRepo content |> value
        let badTree = ObjectWriter.writeTree badRepo [| { Mode = 0o040000; Path = "a"; Hash = badBlob }; { Mode = 0o100644; Path = "b"; Hash = badBlob } |] |> value
        let badSnapshot = { Head = Symbolic "refs/heads/missing"; Refs = Map.ofList [ "refs/heads/test", badTree ]; Digest = "" }
        let report = Fsck.verifyReachable badRepo badSnapshot |> value
        check (not report.IsValid) "fsck missed conflicting edge types on a repeated object"
        let quarantine = Repository.initBare (Path.Combine(root, "overlay.git")) |> value
        let missingCommit = ObjectWriter.writeCommit badRepo { Tree = missing; Parents = [||]; Author = signature; Committer = signature; Message = "missing\n" } |> value
        let overlayReport = Fsck.verifyReachableOverlay badRepo quarantine (Map.ofList [ "refs/heads/test", missingCommit ]) Seq.empty |> value
        check (not overlayReport.IsValid) "full overlay missed missing historical object"
        let malformed = ObjectWriter.writeRaw quarantine "tree" [| 1uy |] |> value
        let extraReport = Fsck.verifyReachableOverlay badRepo quarantine Map.empty [ malformed ] |> value
        check (not extraReport.IsValid) "full overlay accepted malformed unreachable introduced object"
        try PackStream.writeTo repo [| missing |] false false output with _ -> failed <- true
        check failed "streaming missing object succeeded"
        let loose = Repository.getLooseObjectPath repo blob
        let packDir = Repository.getPackDir repo
        Directory.CreateDirectory packDir |> ignore
        let badIndex = Path.Combine(packDir, "pack-bad.idx")
        File.WriteAllBytes(badIndex, [| 1uy; 2uy |])
        let baseContent = Encoding.ASCII.GetBytes "abc"
        let baseHash = Hashing.hashRaw "blob" baseContent
        let delta = [| 3uy; 4uy; 0x90uy; 3uy; 1uy; byte 'd' |]
        use packBody = new MemoryStream()
        packBody.Write("PACK"B)
        packBody.Write([| 0uy; 0uy; 0uy; 2uy; 0uy; 0uy; 0uy; 2uy; 0x76uy |])
        let baseHashBytes = Convert.FromHexString baseHash
        packBody.Write(baseHashBytes, 0, baseHashBytes.Length)
        packBody.Write(Compression.compress delta)
        packBody.WriteByte(0x33uy)
        packBody.Write(Compression.compress baseContent)
        let body = packBody.ToArray()
        let forwardPack = Array.append body (System.Security.Cryptography.SHA1.HashData body)
        let other = Repository.initBare (Path.Combine(root, "other.git")) |> value
        let unpacked = PackWriter.unpackPack other forwardPack |> value
        let target = Hashing.hashRaw "blob" (Encoding.ASCII.GetBytes "abcd")
        check (Array.contains target unpacked) "forward REF_DELTA was not unpacked"
        let third = Repository.initBare (Path.Combine(root, "third.git")) |> value
        use input = new MemoryStream(forwardPack)
        use counting = new PackData.CountingStream(input)
        let streamed = PackWriter.unpackPackStream third counting |> value
        check (Array.contains target streamed) "streamed forward REF_DELTA was not unpacked"
        let fourth = Repository.initBare (Path.Combine(root, "fourth.git")) |> value
        use noReadPastEnd = new NoReadPastEnd(forwardPack)
        use bounded = new PackData.CountingStream(noReadPastEnd)
        check (PackWriter.unpackPackStream fourth bounded |> Result.isOk) "stream unpack read beyond its trailer"
        check (Result.isError (PackWriter.unpackPack fourth (Array.append forwardPack [| 1uy |]))) "byte-array unpack accepted trailing data"
        let damaged = Array.copy forwardPack
        damaged.[damaged.Length - 1] <- damaged.[damaged.Length - 1] ^^^ 1uy
        let rejected = Repository.initBare (Path.Combine(root, "rejected.git")) |> value
        check (Result.isError (PackWriter.unpackPack rejected damaged)) "bad pack checksum was accepted"
        check (Directory.GetDirectories(Repository.getObjectsDir rejected, "tmp-unpack-*").Length = 0) "failed unpack left staging files"
        check (not (ReadObjects.objectExists rejected target)) "bad pack wrote objects before validation"
        check (Result.isError (Gc.repack repo)) "repack accepted bad index"
        check (File.Exists loose && File.Exists badIndex) "failed repack deleted inputs"
        File.Delete badIndex
        Gc.repack repo |> value |> ignore
        check (git repo.GitDir ["cat-file"; "-p"; blob] = "same object") "Git cannot read repacked object"
        git repo.GitDir ["fsck"; "--full"; "--no-reflogs"] |> ignore
    finally
        if Directory.Exists root then Directory.Delete(root, true)
