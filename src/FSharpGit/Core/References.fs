namespace FSharpGit

open System
open System.Collections.Generic
open System.IO
open System.Security.Cryptography
open System.Text
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
                Directory.GetFiles(headsDir, "*", SearchOption.AllDirectories)
                |> Array.map (fun path ->
                    Path.GetRelativePath(headsDir, path).Replace(Path.DirectorySeparatorChar, '/'))
                |> Ok
        with
        | ex -> Error $"Failed to list branches: {ex.Message}"
    
    let listTags (repo: Repo) : Result<string[], string> =
        try
            let tagsDir = Repository.getTagsDir repo
            if not (Directory.Exists tagsDir) then
                Ok [||]
            else
                Directory.GetFiles(tagsDir, "*", SearchOption.AllDirectories)
                |> Array.map (fun path ->
                    Path.GetRelativePath(tagsDir, path).Replace(Path.DirectorySeparatorChar, '/'))
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

    let private isObjectId (value: string) =
        value.Length = 40 && value |> Seq.forall Uri.IsHexDigit

    let isValidName (name: string) =
        not (String.IsNullOrWhiteSpace name)
        && name.StartsWith("refs/", StringComparison.Ordinal)
        && not (name.StartsWith("/", StringComparison.Ordinal))
        && not (name.EndsWith("/", StringComparison.Ordinal))
        && not (name.EndsWith(".", StringComparison.Ordinal))
        && not (name.Contains("..", StringComparison.Ordinal))
        && not (name.Contains("@{", StringComparison.Ordinal))
        && not (name.Contains("//", StringComparison.Ordinal))
        && not (name.Contains('\\'))
        && not (
            name
            |> Seq.exists (fun c ->
                Char.IsControl c
                || Char.IsWhiteSpace c
                || c = '~'
                || c = '^'
                || c = ':'
                || c = '?'
                || c = '*'
                || c = '['))
        && (
            name.Split('/')
            |> Array.forall (fun part ->
                part <> ""
                && not (part.StartsWith(".", StringComparison.Ordinal))
                && not (part.EndsWith(".lock", StringComparison.OrdinalIgnoreCase))))

    let private includesRef policy (name: string) =
        match policy with
        | RefPolicy.Public ->
            name.StartsWith("refs/heads/", StringComparison.Ordinal)
            || name.StartsWith("refs/tags/", StringComparison.Ordinal)
        | RefPolicy.Replication -> name.StartsWith("refs/", StringComparison.Ordinal)

    let private readPackedRefsStrict (repo: Repo) =
        let path = Repository.getPackedRefsFile repo
        if not (File.Exists path) then
            [||]
        else
            File.ReadLines path
            |> Seq.mapi (fun index line -> index + 1, line.Trim())
            |> Seq.choose (fun (lineNumber, line) ->
                if line = "" || line.StartsWith("#", StringComparison.Ordinal) || line.StartsWith("^", StringComparison.Ordinal) then
                    None
                else
                    let separator = line.IndexOf(' ')
                    if separator <= 0 || separator = line.Length - 1 || line.IndexOf(' ', separator + 1) >= 0 then
                        raise (InvalidDataException $"Malformed packed-ref at line {lineNumber}")
                    let hash = line.Substring(0, separator).ToLowerInvariant()
                    let name = line.Substring(separator + 1)
                    if not (isObjectId hash) then
                        raise (InvalidDataException $"Invalid object ID for packed ref {name}")
                    Some(name, hash))
            |> Seq.toArray

    /// Return a deterministic logical snapshot of HEAD and the selected ref
    /// namespace. Loose refs override packed refs, as in Git.
    let snapshot (repo: Repo) (policy: RefPolicy) : Result<RefSnapshot, string> =
        try
            let values = Dictionary<string, RefValue>(StringComparer.Ordinal)
            for name, hash in readPackedRefsStrict repo do
                if includesRef policy name then
                    if not (isValidName name) then
                        raise (InvalidDataException $"Invalid packed ref name: {name}")
                    if values.ContainsKey name then
                        raise (InvalidDataException $"Duplicate packed ref: {name}")
                    values.[name] <- Direct hash

            let refsDir = Repository.getRefsDir repo
            if Directory.Exists refsDir then
                for path in Directory.EnumerateFiles(refsDir, "*", SearchOption.AllDirectories) do
                    let relative = Path.GetRelativePath(repo.GitDir, path).Replace(Path.DirectorySeparatorChar, '/')
                    if includesRef policy relative then
                        if not (isValidName relative) then
                            raise (InvalidDataException $"Invalid loose ref name: {relative}")
                        let value = parseRefValue (File.ReadAllText path)
                        match value with
                        | Direct hash when not (isObjectId hash) ->
                            raise (InvalidDataException $"Invalid object ID for loose ref {relative}")
                        | Symbolic target when not (isValidName target) ->
                            raise (InvalidDataException $"Invalid symbolic target {target} for {relative}")
                        | _ -> ()
                        values.[relative] <-
                            match value with
                            | Direct hash -> Direct(hash.ToLowerInvariant())
                            | symbolic -> symbolic

            let resolve name =
                let seen = HashSet<string>(StringComparer.Ordinal)
                let rec loop current =
                    if not (seen.Add current) then
                        raise (InvalidDataException $"Symbolic ref cycle at {current}")
                    match values.TryGetValue current with
                    | true, Direct hash -> hash
                    | true, Symbolic target -> loop target
                    | _ -> raise (InvalidDataException $"Symbolic ref target not found: {current}")
                loop name

            let resolved =
                values.Keys
                |> Seq.sort
                |> Seq.map (fun name -> name, resolve name)
                |> Map.ofSeq

            let head =
                match readHead repo with
                | Ok (Direct hash) when isObjectId hash -> Direct(hash.ToLowerInvariant())
                | Ok (Direct _) -> raise (InvalidDataException "HEAD contains an invalid object ID")
                | Ok (Symbolic target) when isValidName target -> Symbolic target
                | Ok (Symbolic target) -> raise (InvalidDataException $"HEAD has an invalid symbolic target: {target}")
                | Error error -> raise (InvalidDataException error)

            let canonical = StringBuilder("fsharpgit-ref-snapshot-v1\n")
            match head with
            | Direct hash -> canonical.Append("HEAD\000").Append(hash).Append('\n') |> ignore
            | Symbolic target -> canonical.Append("HEAD\000ref:").Append(target).Append('\n') |> ignore
            for KeyValue(name, hash) in resolved do
                canonical.Append(name).Append('\000').Append(hash).Append('\n') |> ignore
            let digest =
                canonical.ToString()
                |> Encoding.UTF8.GetBytes
                |> SHA256.HashData
                |> Convert.ToHexString
                |> fun value -> value.ToLowerInvariant()

            Ok { Head = head; Refs = resolved; Digest = digest }
        with ex ->
            Error $"Failed to snapshot refs: {ex.Message}"

    let writeDirectAtomic (repo: Repo) (name: string) (hash: GitHash) : Result<unit, string> =
        try
            let normalized = hash.ToLowerInvariant()
            if not (isValidName name) then
                Error $"Invalid ref name: {name}"
            elif not (Hashing.isValidHash normalized) then
                Error $"Invalid object ID: {hash}"
            else
                let path = Path.Combine(repo.GitDir, name.Replace('/', Path.DirectorySeparatorChar))
                Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
                let temporary = path + ".lock-" + Guid.NewGuid().ToString("N")
                try
                    File.WriteAllText(temporary, normalized + "\n")
                    File.Move(temporary, path, true)
                    Ok ()
                finally
                    if File.Exists temporary then File.Delete temporary
        with ex -> Error $"Failed to update ref {name}: {ex.Message}"

    let deleteAtomic (repo: Repo) (name: string) : Result<unit, string> =
        try
            if not (isValidName name) then
                Error $"Invalid ref name: {name}"
            else
                let path = Path.Combine(repo.GitDir, name.Replace('/', Path.DirectorySeparatorChar))
                if File.Exists path then File.Delete path
                let packedPath = Repository.getPackedRefsFile repo
                if File.Exists packedPath then
                    let remaining =
                        readPackedRefsStrict repo
                        |> Array.filter (fun (packedName, _) -> packedName <> name)
                        |> Array.sortBy fst
                    let temporary = packedPath + ".lock-" + Guid.NewGuid().ToString("N")
                    try
                        use writer = new StreamWriter(temporary, false, UTF8Encoding(false))
                        writer.WriteLine("# pack-refs with: sorted")
                        for packedName, packedHash in remaining do
                            writer.WriteLine($"{packedHash} {packedName}")
                        writer.Flush()
                        File.Move(temporary, packedPath, true)
                    finally
                        if File.Exists temporary then File.Delete temporary
                Ok ()
        with ex -> Error $"Failed to delete ref {name}: {ex.Message}"
    
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
            let branchDir = Path.GetDirectoryName branchFile
            if not (String.IsNullOrWhiteSpace branchDir) then
                Directory.CreateDirectory branchDir |> ignore
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
            let branchDir = Path.GetDirectoryName branchFile
            if not (String.IsNullOrWhiteSpace branchDir) then
                Directory.CreateDirectory branchDir |> ignore
            File.WriteAllText(branchFile, hash)
            Ok ()
        with
        | ex -> Error $"Failed to update branch {name}: {ex.Message}"
