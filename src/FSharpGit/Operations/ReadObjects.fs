namespace FSharpGit

open System.IO

module ReadObjects =
    
    let private tryReadLooseObject (repo: Repo) (hash: GitHash) : Result<GitObject, string> =
        try
            let objectPath = Repository.getLooseObjectPath repo hash
            if not (File.Exists objectPath) then
                Error $"Object not found: {hash}"
            else
                use stream = File.OpenRead objectPath
                let data = Array.zeroCreate (int stream.Length)
                stream.Read(data, 0, data.Length) |> ignore
                let decompressed = Compression.decompress data
                ObjectParser.parseObject decompressed
        with
        | ex -> Error $"Failed to read loose object {hash}: {ex.Message}"
    
    /// Reconstruct a parsed object from a raw (type, content) pair.
    let private parseRaw (t: string) (content: byte[]) : Result<GitObject, string> =
        let header = System.Text.Encoding.UTF8.GetBytes(sprintf "%s %d\000" t content.Length)
        ObjectParser.parseObject (Array.append header content)

    let readObject (repo: Repo) (hash: GitHash) : Result<GitObject, string> =
        match tryReadLooseObject repo hash with
        | Ok obj -> Ok obj
        | Error _ ->
            match PackStore.tryRead repo hash with
            | Some (t, content) -> parseRaw t content
            | None -> Error $"Object not found: {hash}"

    /// Read an object's exact (typeName, contentBytes) without parsing, so the
    /// bytes can be re-served or re-packed with an identical hash. Loose objects
    /// preserve their bytes exactly; packed objects fall back to canonical
    /// re-serialization (byte-exact for well-formed objects).
    let readRawObject (repo: Repo) (hash: GitHash) : Result<string * byte[], string> =
        try
            let objectPath = Repository.getLooseObjectPath repo hash
            if File.Exists objectPath then
                let decompressed = Compression.decompress (File.ReadAllBytes objectPath)
                let nul = System.Array.IndexOf(decompressed, 0uy)
                if nul < 0 then Error $"Invalid object (no header null): {hash}"
                else
                    let header = System.Text.Encoding.UTF8.GetString(decompressed, 0, nul)
                    let sp = header.IndexOf(' ')
                    let objType = if sp < 0 then header else header.Substring(0, sp)
                    let content = decompressed.[nul + 1 ..]
                    Ok (objType, content)
            else
                match PackStore.tryRead repo hash with
                | Some raw -> Ok raw
                | None -> Error $"Object not found: {hash}"
        with ex -> Error $"Failed to read raw object {hash}: {ex.Message}"
    
    let readBlob (repo: Repo) (hash: GitHash) : Result<byte[], string> =
        match readObject repo hash with
        | Ok (Blob data) -> Ok data
        | Ok _ -> Error $"Object {hash} is not a blob"
        | Error msg -> Error msg
    
    let readTree (repo: Repo) (hash: GitHash) : Result<TreeEntry[], string> =
        match readObject repo hash with
        | Ok (Tree entries) -> Ok entries
        | Ok _ -> Error $"Object {hash} is not a tree"
        | Error msg -> Error msg
    
    let readCommit (repo: Repo) (hash: GitHash) : Result<CommitData, string> =
        match readObject repo hash with
        | Ok (Commit commit) -> Ok commit
        | Ok _ -> Error $"Object {hash} is not a commit"
        | Error msg -> Error msg
    
    let readTag (repo: Repo) (hash: GitHash) : Result<TagData, string> =
        match readObject repo hash with
        | Ok (Tag tag) -> Ok tag
        | Ok _ -> Error $"Object {hash} is not a tag"
        | Error msg -> Error msg
    
    let objectExists (repo: Repo) (hash: GitHash) : bool =
        File.Exists (Repository.getLooseObjectPath repo hash) || PackStore.exists repo hash
    
    let private objectCache = System.Collections.Concurrent.ConcurrentDictionary<Repo * GitHash, GitObject>()
    
    let readObjectCached (repo: Repo) (hash: GitHash) : Result<GitObject, string> =
        let key = (repo, hash)
        match objectCache.TryGetValue(key) with
        | true, obj -> Ok obj
        | false, _ ->
            match readObject repo hash with
            | Ok obj -> 
                objectCache.[key] <- obj
                Ok obj
            | Error msg -> Error msg
    
    let clearCache (repo: Repo) : unit =
        let keysToRemove = objectCache.Keys |> Seq.filter (fun (r, _) -> r = repo) |> Seq.toArray
        for key in keysToRemove do
            objectCache.TryRemove(key) |> ignore
