namespace Rbt.Git

open System
open System.IO
open System.Security.Cryptography
open System.Text

module Hashing =
    
    let sha1 (data: byte[]) : GitHash =
        use sha1 = SHA1.Create()
        let hash = sha1.ComputeHash data
        BitConverter.ToString(hash).Replace("-", "").ToLowerInvariant()
    
    let sha1String (str: string) : GitHash =
        let bytes = Encoding.UTF8.GetBytes str
        sha1 bytes
    
    let private isLowerHex (c: char) : bool =
        (c >= 'a' && c <= 'f') || (c >= '0' && c <= '9')
    
    let isValidHash (hash: string) : bool =
        hash.Length = 40 && hash |> Seq.forall isLowerHex
    
    let parseHash (str: string) : Result<GitHash, string> =
        if isValidHash str then
            Ok (str.ToLowerInvariant())
        else
            Error $"Invalid git hash format: {str}"
    
    let hashRaw (objType: string) (content: byte[]) : GitHash =
        let header = Encoding.UTF8.GetBytes(sprintf "%s %d\000" objType content.Length)
        use hasher = IncrementalHash.CreateHash(HashAlgorithmName.SHA1)
        hasher.AppendData(header)
        hasher.AppendData(content)
        Convert.ToHexString(hasher.GetHashAndReset()).ToLowerInvariant()

    let hashBlob (data: byte[]) : GitHash = hashRaw "blob" data

    let private signatureLine (label: string) (signature: Signature) =
        let minutes = int signature.Time.Offset.TotalMinutes
        let sign = if minutes < 0 then "-" else "+"
        sprintf "%s %s <%s> %d %s%02d%02d\n"
            label signature.Name signature.Email (signature.Time.ToUnixTimeSeconds())
            sign (abs minutes / 60) (abs minutes % 60)

    let private hashBytes (hash: GitHash) =
        if hash.Length <> 40 then invalidArg "hash" "Expected a 40-character object ID"
        let bytes = Array.zeroCreate<byte> 20
        for i in 0 .. 19 do
            bytes.[i] <- Convert.ToByte(hash.Substring(i * 2, 2), 16)
        bytes

    /// Git compares tree names bytewise, treating a directory as ending in '/'.
    let serializeTree (entries: TreeEntry[]) : byte[] =
        let keyed =
            entries |> Array.map (fun entry ->
                let name = Encoding.UTF8.GetBytes entry.Path
                let key = if entry.Mode = 0o40000 then Array.append name [| byte '/' |] else name
                entry, key)
        let compare (_, x: byte[]) (_, y: byte[]) =
            let shared = min x.Length y.Length
            let mutable index = 0
            while index < shared && x.[index] = y.[index] do
                index <- index + 1
            if index = shared then Operators.compare x.Length y.Length
            else Operators.compare x.[index] y.[index]
        Array.sortInPlaceWith compare keyed
        use output = new MemoryStream()
        for entry, _ in keyed do
            let prefix = Encoding.UTF8.GetBytes(sprintf "%s %s\000" (Convert.ToString(entry.Mode, 8)) entry.Path)
            output.Write prefix
            output.Write(hashBytes entry.Hash)
        output.ToArray()

    let serializeCommit (commit: CommitData) : byte[] =
        let text = StringBuilder()
        text.Append("tree ").Append(commit.Tree).Append('\n') |> ignore
        for parent in commit.Parents do
            text.Append("parent ").Append(parent).Append('\n') |> ignore
        text.Append(signatureLine "author" commit.Author) |> ignore
        text.Append(signatureLine "committer" commit.Committer) |> ignore
        text.Append('\n').Append(commit.Message) |> ignore
        Encoding.UTF8.GetBytes(text.ToString())

    let serializeTag (tag: TagData) : byte[] =
        let text = StringBuilder()
        text.Append("object ").Append(tag.Object).Append('\n') |> ignore
        text.Append("type ").Append(tag.ObjectType).Append('\n') |> ignore
        text.Append("tag ").Append(tag.Tag).Append('\n') |> ignore
        text.Append(signatureLine "tagger" tag.Tagger) |> ignore
        text.Append('\n').Append(tag.Message) |> ignore
        Encoding.UTF8.GetBytes(text.ToString())

    let serialize (obj: GitObject) : string * byte[] =
        match obj with
        | Blob data -> "blob", data
        | Tree entries -> "tree", serializeTree entries
        | Commit commit -> "commit", serializeCommit commit
        | Tag tag -> "tag", serializeTag tag

    let hashTree (entries: TreeEntry[]) : GitHash = hashRaw "tree" (serializeTree entries)
    let hashCommit (commit: CommitData) : GitHash = hashRaw "commit" (serializeCommit commit)
