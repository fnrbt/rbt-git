namespace Rbt.Git

open System
open System.IO
open System.Text
open System.Text.RegularExpressions

module ObjectParser =
    
    let private parseHeader (data: byte[]) : string * byte[] =
        let nullIndex = Array.IndexOf(data, 0uy)
        if nullIndex < 0 then
            failwith "Invalid object format: no null byte found"
        
        let header = Encoding.UTF8.GetString(data.[0..nullIndex])
        let content = data.[nullIndex + 1..]
        header, content
    
    let private parseSignature (str: string) : Signature =
        let regex = Regex(@"^(.+) <(.+)> (\d+) ([+-]\d{4})$")
        let m = regex.Match str
        if not m.Success then
            failwith $"Invalid signature format: {str}"
        
        let name = m.Groups.[1].Value
        let email = m.Groups.[2].Value
        let time = int64 m.Groups.[3].Value
        let offset = int m.Groups.[4].Value
        let hours = offset / 100
        let minutes = abs (offset % 100)
        let timeZoneOffset = TimeSpan(hours, minutes, 0)
        let dateTime = DateTimeOffset.FromUnixTimeSeconds time
        let dateTime = dateTime.ToOffset timeZoneOffset
        
        { Name = name; Email = email; Time = dateTime }
    
    let parseBlob (data: byte[]) : byte[] =
        let header, content = parseHeader data
        if not (header.StartsWith "blob ") then
            failwith $"Not a blob object: {header}"
        content
    
    let parseTree (data: byte[]) : TreeEntry[] =
        let header, content = parseHeader data
        if not (header.StartsWith "tree ") then
            failwith $"Not a tree object: {header}"
        
        let rec parseEntries (offset: int) (acc: TreeEntry list) : TreeEntry list =
            if offset >= content.Length then
                List.rev acc
            else
                let spaceIndex = Array.IndexOf(content, 32uy, offset)
                let nullIndex = Array.IndexOf(content, 0uy, spaceIndex)
                
                let modeStr = Encoding.UTF8.GetString(content.[offset..spaceIndex - 1])
                let mode = Convert.ToInt32(modeStr, 8)
                let path = Encoding.UTF8.GetString(content.[spaceIndex + 1..nullIndex - 1])
                let hash = BitConverter.ToString(content.[nullIndex + 1..nullIndex + 20]).Replace("-", "").ToLowerInvariant()
                
                parseEntries (nullIndex + 21) ({ Mode = mode; Path = path; Hash = hash } :: acc)
        
        parseEntries 0 [] |> List.toArray
    
    let parseCommit (data: byte[]) : CommitData =
        let header, content = parseHeader data
        if not (header.StartsWith "commit ") then
            failwith $"Not a commit object: {header}"
        
        let lines = Encoding.UTF8.GetString(content).Split('\n')
        let rec parseCommitLines index (tree: string option) (parents: string list) (author: Signature option) (committer: Signature option) (message: StringBuilder) =
            if index >= lines.Length then
                let messageStr = message.ToString().TrimStart()
                { Tree = tree.Value
                  Parents = List.toArray parents
                  Author = author.Value
                  Committer = committer.Value
                  Message = messageStr }
            else
                let line = lines.[index]
                if line.StartsWith "tree " then
                    parseCommitLines (index + 1) (Some line.[5..]) parents author committer message
                elif line.StartsWith "parent " then
                    parseCommitLines (index + 1) tree (line.[7..] :: parents) author committer message
                elif line.StartsWith "author " then
                    let signature = parseSignature line.[7..]
                    parseCommitLines (index + 1) tree parents (Some signature) committer message
                elif line.StartsWith "committer " then
                    let signature = parseSignature line.[10..]
                    parseCommitLines (index + 1) tree parents author (Some signature) message
                elif String.IsNullOrWhiteSpace line then
                    if index + 1 < lines.Length then
                        message.AppendLine(String.Join("\n", lines.[index + 1..])) |> ignore
                    parseCommitLines lines.Length tree parents author committer message
                else
                    parseCommitLines (index + 1) tree parents author committer message
        
        parseCommitLines 0 None [] None None (StringBuilder())
    
    let parseTag (data: byte[]) : TagData =
        let header, content = parseHeader data
        if not (header.StartsWith "tag ") then
            failwith $"Not a tag object: {header}"
        
        let lines = Encoding.UTF8.GetString(content).Split('\n')
        let rec parseTagLines index (obj: string option) (objType: string option) (tag: string option) (tagger: Signature option) (message: StringBuilder) =
            if index >= lines.Length then
                let messageStr = message.ToString().TrimStart()
                { Object = obj.Value
                  ObjectType = objType.Value
                  Tag = tag.Value
                  Tagger = tagger.Value
                  Message = messageStr }
            else
                let line = lines.[index]
                if line.StartsWith "object " then
                    parseTagLines (index + 1) (Some line.[7..]) objType tag tagger message
                elif line.StartsWith "type " then
                    parseTagLines (index + 1) obj (Some line.[5..]) tag tagger message
                elif line.StartsWith "tag " then
                    parseTagLines (index + 1) obj objType (Some line.[4..]) tagger message
                elif line.StartsWith "tagger " then
                    let signature = parseSignature line.[7..]
                    parseTagLines (index + 1) obj objType tag (Some signature) message
                elif String.IsNullOrWhiteSpace line then
                    if index + 1 < lines.Length then
                        message.AppendLine(String.Join("\n", lines.[index + 1..])) |> ignore
                    parseTagLines lines.Length obj objType tag tagger message
                else
                    parseTagLines (index + 1) obj objType tag tagger message
        
        parseTagLines 0 None None None None (StringBuilder())
    
    let parseObject (data: byte[]) : Result<GitObject, string> =
        try
            let header, _ = parseHeader data
            if header.StartsWith "blob " then
                Ok (Blob (parseBlob data))
            elif header.StartsWith "tree " then
                Ok (Tree (parseTree data))
            elif header.StartsWith "commit " then
                Ok (Commit (parseCommit data))
            elif header.StartsWith "tag " then
                Ok (GitObject.Tag (parseTag data))
            else
                Error $"Unknown object type: {header}"
        with
        | ex -> Error $"Failed to parse object: {ex.Message}"
