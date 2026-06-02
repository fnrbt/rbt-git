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
    
    let readObject (repo: Repo) (hash: GitHash) : Result<GitObject, string> =
        match tryReadLooseObject repo hash with
        | Ok obj -> Ok obj
        | Error _ ->
            match PackParser.findPackObject repo hash with
            | Ok (obj, _) -> Ok obj
            | Error msg -> Error msg
    
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
        let objectPath = Repository.getLooseObjectPath repo hash
        if File.Exists objectPath then
            true
        else
            match PackParser.findPackObject repo hash with
            | Ok _ -> true
            | _ -> false
    
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
