module ParsingRegression

open System
open System.Diagnostics
open System.IO
open System.Security.Cryptography
open System.Text
open Rbt.Git

let private check condition message = if not condition then failwith message
let private value = function Ok x -> x | Error error -> failwith error
let private rejects action =
    try action (); failwith "Corrupted input was accepted"
    with ex when ex.Message <> "Corrupted input was accepted" -> ()

let private git (arguments: string) (input: byte[]) =
    let start = ProcessStartInfo("git", arguments)
    start.RedirectStandardInput <- true
    start.RedirectStandardOutput <- true
    start.RedirectStandardError <- true
    use gitProcess = Process.Start start
    gitProcess.StandardInput.BaseStream.Write input
    gitProcess.StandardInput.Close()
    let result = gitProcess.StandardOutput.ReadToEnd().Trim()
    let error = gitProcess.StandardError.ReadToEnd()
    gitProcess.WaitForExit()
    if gitProcess.ExitCode <> 0 then failwithf "git %s: %s" arguments error
    result

let private raw objectType (data: byte[]) =
    let header = Encoding.UTF8.GetBytes(sprintf "%s %d\000" objectType data.Length)
    Array.append header data

let private withRepo test =
    let path = Path.Combine(Path.GetTempPath(), "rbt-parsing-" + Guid.NewGuid().ToString("N"))
    try
        let repo = Repository.initBare path |> value
        test repo
    finally
        if Directory.Exists path then Directory.Delete(path, true)

let private rewriteIndex repo mutate =
    let path = Repository.getIndexPath repo
    let bytes = File.ReadAllBytes path
    mutate bytes
    let checksum = SHA1.HashData(bytes.AsSpan(0, bytes.Length - 20))
    checksum.CopyTo(bytes, bytes.Length - 20)
    File.WriteAllBytes(path, bytes)

let run () =
    let blobId = Hashing.hashBlob (Encoding.UTF8.GetBytes "x")
    let tree = [| { Mode = 0o100644; Path = "é.txt"; Hash = blobId } |]
    let treeBytes = Hashing.serializeTree tree
    check (Hashing.hashTree tree = git "hash-object -t tree --stdin" treeBytes) "Git tree hash differs"
    let author = { Name = "Test  Identity"; Email = "test@example.invalid"; Time = DateTimeOffset.FromUnixTimeSeconds(123L).ToOffset(TimeSpan.FromMinutes(-330.0)) }
    let commit = { Tree = Hashing.hashTree tree; Parents = [| blobId; String.replicate 40 "a" |]; Author = author; Committer = author; Message = "  leading\n\ntrailing\n" }
    let bytes = Hashing.serializeCommit commit
    check (Hashing.hashCommit commit = git "hash-object -t commit --stdin" bytes) "Git commit hash differs"
    let parsed = ObjectParser.parseCommit (raw "commit" bytes)
    check (parsed.Parents = commit.Parents && parsed.Message = commit.Message && parsed.Author.Time.Offset = author.Time.Offset) "Commit order, message or timezone changed"
    let tag = { Object = blobId; ObjectType = "blob"; Tag = "v1"; Tagger = author; Message = "  tag\n" }
    check ((ObjectParser.parseTag (raw "tag" (Hashing.serializeTag tag))).Message = tag.Message) "Tag message changed"
    rejects (fun () -> ObjectParser.parseBlob (Encoding.UTF8.GetBytes "blob 9\000x") |> ignore)
    withRepo (fun repo ->
        let path = Repository.getIndexPath repo
        let entry = { Path = "é-file"; Hash = blobId; Mode = 0o100644; ModifiedTime = DateTimeOffset.FromUnixTimeSeconds(456L); Size = 5L; Stage = 0 }
        IndexParser.writeIndex repo { Version = 2; Entries = [| entry |] } |> value
        let parsed = IndexParser.readIndex repo |> value
        check (parsed.Entries.[0] = entry) "Index roundtrip changed endian metadata or pathname"
        let gitEntry = git (sprintf "--git-dir=\"%s\" ls-files --stage -z" repo.GitDir) [||]
        check (gitEntry.Contains(blobId) && gitEntry.Contains(entry.Path)) "Git could not read written index"
        let original = File.ReadAllBytes path
        let version3 = Array.copy original
        version3.[7] <- 3uy
        version3.[72] <- version3.[72] ||| 0x40uy
        Array.Copy(version3, 74, version3, 76, 8)
        version3.[74] <- 0uy
        version3.[75] <- 0uy
        SHA1.HashData(version3.AsSpan(0, version3.Length - 20)).CopyTo(version3, version3.Length - 20)
        File.WriteAllBytes(path, version3)
        check ((IndexParser.readIndex repo |> value).Entries.[0].Path = entry.Path) "Version 3 extended flags lost path"
        version3.[75] <- 1uy
        SHA1.HashData(version3.AsSpan(0, version3.Length - 20)).CopyTo(version3, version3.Length - 20)
        File.WriteAllBytes(path, version3)
        check (match IndexParser.readIndex repo with Error _ -> true | _ -> false) "Unsupported extended flags accepted"
        File.WriteAllBytes(path, original)
        // Optional TREE extension: size includes only payload, followed by checksum.
        let extension = Array.append (Encoding.ASCII.GetBytes "TREE") [| 0uy; 0uy; 0uy; 1uy; 42uy |]
        let baseLength = original.Length - 20
        let extended = Array.zeroCreate<byte> (original.Length + extension.Length)
        Buffer.BlockCopy(original, 0, extended, 0, baseLength)
        Buffer.BlockCopy(extension, 0, extended, baseLength, extension.Length)
        SHA1.HashData(extended.AsSpan(0, extended.Length - 20)).CopyTo(extended, extended.Length - 20)
        File.WriteAllBytes(path, extended)
        check ((IndexParser.readIndex repo |> value).Entries.Length = 1) "Optional index extension rejected"
        extended.[baseLength] <- byte 'l'
        SHA1.HashData(extended.AsSpan(0, extended.Length - 20)).CopyTo(extended, extended.Length - 20)
        File.WriteAllBytes(path, extended)
        check (match IndexParser.readIndex repo with Error _ -> true | _ -> false) "Required index extension accepted"
        File.WriteAllBytes(path, original)
        rewriteIndex repo (fun data -> data.[7] <- 4uy)
        check (match IndexParser.readIndex repo with Error _ -> true | _ -> false) "Version 4 index misparsed"
        File.WriteAllBytes(path, original)
        rewriteIndex repo (fun data -> data.[12 + 62 + Encoding.UTF8.GetByteCount(entry.Path)] <- 1uy)
        check (match IndexParser.readIndex repo with Error _ -> true | _ -> false) "Invalid path NUL accepted"
        File.WriteAllBytes(path, original)
        original.[original.Length - 1] <- original.[original.Length - 1] ^^^ 1uy
        File.WriteAllBytes(path, original)
        check (match IndexParser.readIndex repo with Error _ -> true | _ -> false) "Index checksum corruption accepted"
        IndexParser.writeIndex repo { Version = 2; Entries = [| { entry with Path = String.replicate 4096 "x" } |] } |> value
        check ((IndexParser.readIndex repo |> value).Entries.[0].Path.Length = 4096) "Long index path was truncated"
        rewriteIndex repo (fun data -> data.[7] <- 3uy)
        check ((IndexParser.readIndex repo |> value).Version = 3) "Version 3 index rejected"
        let indexPath = Path.Combine(repo.GitDir, "objects", "pack", "test.idx")
        Directory.CreateDirectory(Path.GetDirectoryName indexPath) |> ignore
        let empty = Array.zeroCreate<byte> (8 + 256 * 4 + 40)
        empty.[0..7] <- [| 0xffuy; 0x74uy; 0x4fuy; 0x63uy; 0uy; 0uy; 0uy; 2uy |]
        SHA1.HashData(empty.AsSpan(0, empty.Length - 20)).CopyTo(empty, empty.Length - 20)
        File.WriteAllBytes(indexPath, empty)
        check ((PackParser.readPackIndex indexPath |> value).Objects.Length = 0) "Empty v2 pack index rejected"
        empty.[7] <- 3uy
        SHA1.HashData(empty.AsSpan(0, empty.Length - 20)).CopyTo(empty, empty.Length - 20)
        File.WriteAllBytes(indexPath, empty)
        check (match PackParser.readPackIndex indexPath with Error _ -> true | _ -> false) "Unsupported pack index accepted")
    let malformed = [| [| 0x80uy |]; [| 0xffuy; 0xffuy; 0xffuy; 0xffuy; 0xffuy; 0xffuy |] |]
    for header in malformed do
        rejects (fun () -> PackData.readObjHeader header 0 |> ignore)
        use stream = new MemoryStream(header)
        rejects (fun () -> PackData.readObjHeaderFromStream stream |> ignore)
    rejects (fun () -> PackData.readOfsBase [| 0x80uy |] 0 |> ignore)
    rejects (fun () -> PackData.applyDelta [| 0uy |] [| 2uy; 1uy; 1uy; 42uy |] |> ignore)
    rejects (fun () -> PackData.applyDelta [| 0uy |] [| 1uy; 2uy; 1uy; 42uy |] |> ignore)
    rejects (fun () -> PackData.applyDelta [| 0uy |] [| 1uy; 1uy; 0x91uy; 2uy; 1uy |] |> ignore)
    rejects (fun () -> PackData.inflateAt (Compression.compress [| 1uy |]) 0 2 |> ignore)
    let compressed = Compression.compress (Encoding.UTF8.GetBytes "payload")
    compressed.[compressed.Length - 1] <- compressed.[compressed.Length - 1] ^^^ 1uy
    rejects (fun () -> Compression.decompress compressed |> ignore)
    for content in [| [||]; Encoding.UTF8.GetBytes "payload" |] do
        let memberBytes = Compression.compress content
        let inflated, compressedLength = PackData.inflateAt memberBytes 0 content.Length
        check (inflated = content && compressedLength = memberBytes.Length) "Valid zlib member did not inflate exactly"
        use complete = new MemoryStream(memberBytes)
        check (PackData.inflateFromStream complete content.Length = content) "Valid zlib stream member rejected"
        for length in 1 .. memberBytes.Length - 1 do
            let truncated = memberBytes.[0..length - 1]
            rejects (fun () -> PackData.inflateAt truncated 0 content.Length |> ignore)
            use stream = new MemoryStream(truncated)
            rejects (fun () -> PackData.inflateFromStream stream content.Length |> ignore)
            rejects (fun () -> Compression.decompress truncated |> ignore)
