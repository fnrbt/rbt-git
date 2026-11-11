namespace Rbt.Git

open System
open System.IO
open System.Text
open System.Text.RegularExpressions

module ObjectParser =
    let private utf8 = UTF8Encoding(false, true)
    
    let private parseHeader (data: byte[]) : string * byte[] =
        let nullIndex = Array.IndexOf(data, 0uy)
        if nullIndex < 0 then failwith "Invalid object format: missing header delimiter"
        let header = Encoding.ASCII.GetString(data, 0, nullIndex)
        let space = header.IndexOf ' '
        if space < 1 then failwith "Invalid object header"
        let mutable length = 0L
        if not (Int64.TryParse(header.Substring(space + 1), &length))
           || length <> int64 (data.Length - nullIndex - 1) then
            failwith "Object content length mismatch"
        header, data.[nullIndex + 1..]

    let private parseSignature (str: string) : Signature =
        let matched = Regex.Match(str, @"^(.*) <([^<>]*)> (-?\d+) ([+-])(\d{2})(\d{2})$")
        if not matched.Success then failwith $"Invalid signature format: {str}"
        let hours = int matched.Groups.[5].Value
        let minutes = int matched.Groups.[6].Value
        if hours > 14 || minutes >= 60 || (hours = 14 && minutes <> 0) then
            failwith "Invalid signature timezone"
        let offset = TimeSpan(hours, minutes, 0)
        let offset = if matched.Groups.[4].Value = "-" then -offset else offset
        let dateTime = DateTimeOffset.FromUnixTimeSeconds(Int64.Parse matched.Groups.[3].Value).ToOffset offset
        { Name = matched.Groups.[1].Value; Email = matched.Groups.[2].Value; Time = dateTime }
    
    let parseBlob (data: byte[]) : byte[] =
        let header, content = parseHeader data
        if not (header.StartsWith "blob ") then
            failwith $"Not a blob object: {header}"
        content
    
    let parseTree (data: byte[]) : TreeEntry[] =
        let header, content = parseHeader data
        if not (header.StartsWith "tree ") then
            failwith $"Not a tree object: {header}"
        
        let entries = ResizeArray<TreeEntry>()
        let mutable offset = 0
        while offset < content.Length do
            let spaceIndex = Array.IndexOf(content, 32uy, offset)
            if spaceIndex < offset || spaceIndex = offset then failwith "Invalid tree mode"
            let nullIndex = Array.IndexOf(content, 0uy, spaceIndex + 1)
            if nullIndex < 0 || nullIndex + 21 > content.Length then failwith "Truncated tree entry"
            let mode = Convert.ToInt32(Encoding.ASCII.GetString(content, offset, spaceIndex - offset), 8)
            let path = utf8.GetString(content, spaceIndex + 1, nullIndex - spaceIndex - 1)
            let hash = Convert.ToHexString(content, nullIndex + 1, 20).ToLowerInvariant()
            entries.Add { Mode = mode; Path = path; Hash = hash }
            offset <- nullIndex + 21
        entries.ToArray()
    
    let private parseMessageObject (content: byte[]) =
        let text = utf8.GetString content
        let separator = text.IndexOf("\n\n", StringComparison.Ordinal)
        if separator < 0 then failwith "Missing object message delimiter"
        text.Substring(0, separator).Split('\n'), text.Substring(separator + 2)

    let parseCommit (data: byte[]) : CommitData =
        let header, content = parseHeader data
        if not (header.StartsWith "commit ") then failwith $"Not a commit object: {header}"
        let lines, message = parseMessageObject content
        let mutable tree = None
        let parents = ResizeArray<GitHash>()
        let mutable author = None
        let mutable committer = None
        for line in lines do
            if line.StartsWith "tree " then tree <- Some line.[5..]
            elif line.StartsWith "parent " then parents.Add line.[7..]
            elif line.StartsWith "author " then author <- Some(parseSignature line.[7..])
            elif line.StartsWith "committer " then committer <- Some(parseSignature line.[10..])
        { Tree = Option.defaultWith (fun () -> failwith "Missing commit tree") tree
          Parents = parents.ToArray()
          Author = Option.defaultWith (fun () -> failwith "Missing commit author") author
          Committer = Option.defaultWith (fun () -> failwith "Missing commit committer") committer
          Message = message }
    
    let parseTag (data: byte[]) : TagData =
        let header, content = parseHeader data
        if not (header.StartsWith "tag ") then
            failwith $"Not a tag object: {header}"
        
        let lines, message = parseMessageObject content
        let mutable obj = None
        let mutable objType = None
        let mutable tag = None
        let mutable tagger = None
        for line in lines do
            if line.StartsWith "object " then obj <- Some line.[7..]
            elif line.StartsWith "type " then objType <- Some line.[5..]
            elif line.StartsWith "tag " then tag <- Some line.[4..]
            elif line.StartsWith "tagger " then tagger <- Some(parseSignature line.[7..])
        { Object = Option.defaultWith (fun () -> failwith "Missing tag object") obj
          ObjectType = Option.defaultWith (fun () -> failwith "Missing tag type") objType
          Tag = Option.defaultWith (fun () -> failwith "Missing tag name") tag
          Tagger = Option.defaultWith (fun () -> failwith "Missing tagger") tagger
          Message = message }
    
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
