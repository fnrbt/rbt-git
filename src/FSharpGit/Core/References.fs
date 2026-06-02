namespace FSharpGit

open System
open System.IO
open System.Text.RegularExpressions

module References =
    
    let private parseRefValue (content: string) : RefValue =
        if content.StartsWith "ref:" then
            Symbolic (content.[4..].Trim())
        else
            Direct (content.Trim())
    
    let readHead (repo: Repo) : Result<RefValue, string> =
        try
            let headFile = Repository.getHeadFile repo
            if not (File.Exists headFile) then
                Error "HEAD file not found"
            else
                let content = File.ReadAllText headFile
                Ok (parseRefValue content)
        with
        | ex -> Error $"Failed to read HEAD: {ex.Message}"
    
    let readReference (repo: Repo) (refPath: string) : Result<RefValue, string> =
        try
            let fullPath = Path.Combine(Repository.getRefsDir repo, refPath)
            if not (File.Exists fullPath) then
                Error $"Reference not found: {refPath}"
            else
                let content = File.ReadAllText fullPath
                Ok (Direct (content.Trim()))
        with
        | ex -> Error $"Failed to read reference {refPath}: {ex.Message}"
    
    let resolveReference (repo: Repo) (refName: string) : Result<GitHash, string> =
        let rec resolve (ref: string) : Result<GitHash, string> =
            match readReference repo ref with
            | Ok (Direct hash) -> Ok hash
            | Ok (Symbolic target) -> resolve target
            | Error msg -> Error msg
        
        resolve refName
    
    let listBranches (repo: Repo) : Result<string[], string> =
        try
            let headsDir = Repository.getHeadsDir repo
            if not (Directory.Exists headsDir) then
                Ok [||]
            else
                Directory.GetFiles(headsDir)
                |> Array.map Path.GetFileName
                |> Ok
        with
        | ex -> Error $"Failed to list branches: {ex.Message}"
    
    let listTags (repo: Repo) : Result<string[], string> =
        try
            let tagsDir = Repository.getTagsDir repo
            if not (Directory.Exists tagsDir) then
                Ok [||]
            else
                Directory.GetFiles(tagsDir)
                |> Array.map Path.GetFileName
                |> Ok
        with
        | ex -> Error $"Failed to list tags: {ex.Message}"
    
    let listRemotes (repo: Repo) : Result<string[], string> =
        try
            let remotesDir = Repository.getRemotesDir repo
            if not (Directory.Exists remotesDir) then
                Ok [||]
            else
                Directory.GetDirectories(remotesDir)
                |> Array.map Path.GetFileName
                |> Ok
        with
        | ex -> Error $"Failed to list remotes: {ex.Message}"
    
    let private parsePackedRefs (content: string) : (string * GitHash) [] =
        let lines = content.Split('\n')
        lines
        |> Array.choose (fun line ->
            if line.StartsWith "#" || String.IsNullOrWhiteSpace line then
                None
            else
                let parts = line.Split(' ', 2)
                if parts.Length >= 2 then
                    Some (parts.[1], parts.[0])
                else
                    None)
    
    let readPackedRefs (repo: Repo) : Result<(string * GitHash)[], string> =
        try
            let packedRefsFile = Repository.getPackedRefsFile repo
            if not (File.Exists packedRefsFile) then
                Ok [||]
            else
                let content = File.ReadAllText packedRefsFile
                Ok (parsePackedRefs content)
        with
        | ex -> Error $"Failed to read packed refs: {ex.Message}"
    
    let findRef (repo: Repo) (hash: GitHash) : Result<string option, string> =
        try
            let allRefs = seq {
                yield! readPackedRefs repo |> Result.defaultWith (fun _ -> [||]) |> Seq.map (fun (ref, _) -> ref)
                
                match listBranches repo with
                | Ok branches -> yield! branches |> Seq.map (fun b -> $"refs/heads/{b}")
                | _ -> ()
                
                match listTags repo with
                | Ok tags -> yield! tags |> Seq.map (fun t -> $"refs/tags/{t}")
                | _ -> ()
            }
            
            allRefs
            |> Seq.tryFind (fun ref ->
                match resolveReference repo ref with
                | Ok h -> h = hash
                | _ -> false)
            |> Ok
        with
        | ex -> Error $"Failed to find reference: {ex.Message}"
    
    let createBranch (repo: Repo) (name: string) (startPoint: GitHash) : Result<unit, string> =
        try
            let headsDir = Repository.getHeadsDir repo
            if not (Directory.Exists headsDir) then
                Directory.CreateDirectory headsDir |> ignore
            
            let branchFile = Repository.branchFilePath repo name
            File.WriteAllText(branchFile, startPoint)
            Ok ()
        with
        | ex -> Error $"Failed to create branch {name}: {ex.Message}"
    
    let deleteBranch (repo: Repo) (name: string) : Result<unit, string> =
        try
            let branchFile = Repository.branchFilePath repo name
            if not (File.Exists branchFile) then
                Error $"Branch not found: {name}"
            else
                File.Delete branchFile
                Ok ()
        with
        | ex -> Error $"Failed to delete branch {name}: {ex.Message}"
    
    let currentBranch (repo: Repo) : Result<string option, string> =
        match readHead repo with
        | Ok (Symbolic ref) ->
            if ref.StartsWith "refs/heads/" then
                Ok (Some ref.[11..])
            else
                Ok None
        | Ok (Direct _) -> Ok None
        | Error msg -> Error msg
    
    let updateHead (repo: Repo) (refValue: RefValue) : Result<unit, string> =
        try
            let headFile = Repository.getHeadFile repo
            match refValue with
            | Direct hash -> File.WriteAllText(headFile, hash)
            | Symbolic ref -> File.WriteAllText(headFile, $"ref: {ref}")
            Ok ()
        with
        | ex -> Error $"Failed to update HEAD: {ex.Message}"
    
    let updateBranch (repo: Repo) (name: string) (hash: GitHash) : Result<unit, string> =
        try
            let branchFile = Repository.branchFilePath repo name
            File.WriteAllText(branchFile, hash)
            Ok ()
        with
        | ex -> Error $"Failed to update branch {name}: {ex.Message}"
