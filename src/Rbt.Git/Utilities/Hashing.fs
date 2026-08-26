namespace Rbt.Git

open System
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
    
    let hashBlob (data: byte[]) : GitHash =
        let headerText = $"blob {data.Length}\0"
        let header = Encoding.UTF8.GetBytes headerText
        let combined = Array.concat [| header; data |]
        sha1 combined
    
    let hashTree (entries: TreeEntry[]) : GitHash =
        let sb = Text.StringBuilder()
        for entry in entries do
            sb.AppendFormat(" {0:o} {1}\0", entry.Mode, entry.Path) |> ignore
        let headerPart = Encoding.UTF8.GetBytes (sb.ToString())
        let hashParts = entries |> Array.collect (fun entry -> Seq.toArray (entry.Hash |> Seq.map (fun c -> byte c)))
        let combined = Array.concat [| headerPart; hashParts |]
        sha1 combined
    
    let hashCommit (commit: CommitData) : GitHash =
        let sb = Text.StringBuilder()
        sb.AppendLine $"tree {commit.Tree}" |> ignore
        for parent in commit.Parents do
            sb.AppendLine $"parent {parent}" |> ignore
        let authorOffset = (int commit.Author.Time.Offset.TotalMinutes).ToString("+0000;-0000")
        let committerOffset = (int commit.Committer.Time.Offset.TotalMinutes).ToString("+0000;-0000")
        sb.AppendLine $"author {commit.Author.Name} <{commit.Author.Email}> {commit.Author.Time.ToUnixTimeSeconds()} {authorOffset}" |> ignore
        sb.AppendLine $"committer {commit.Committer.Name} <{commit.Committer.Email}> {commit.Committer.Time.ToUnixTimeSeconds()} {committerOffset}" |> ignore
        sb.AppendLine() |> ignore
        sb.Append commit.Message |> ignore
        let commitStr = sb.ToString()
        let headerText = $"commit {commitStr.Length}\0"
        let header = Encoding.UTF8.GetBytes headerText
        let combined = Array.concat [| header; Encoding.UTF8.GetBytes commitStr |]
        sha1 combined
