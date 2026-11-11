namespace Rbt.Git

open System
open System.IO
open System.Text
open System.Collections.Generic
open System.Security.Cryptography

module References =
    let private isObjectId (value: string) =
        not (isNull value) && value.Length = 40 && (value |> Seq.forall Uri.IsHexDigit)

    let isValidName (name: string) =
        not (String.IsNullOrWhiteSpace name)
        && name.StartsWith("refs/", StringComparison.Ordinal)
        && not (name.EndsWith(".", StringComparison.Ordinal))
        && not (name.Contains("..", StringComparison.Ordinal))
        && not (name.Contains("@{", StringComparison.Ordinal))
        && not (name |> Seq.exists (fun c -> Char.IsControl c || Char.IsWhiteSpace c || "\\~^:?*[".Contains(c)))
        && (name.Split('/') |> Array.forall (fun p -> p <> "" && not (p.StartsWith(".")) && not (p.EndsWith(".lock", StringComparison.OrdinalIgnoreCase))))
        && name.Split('/').Length >= 2

    let private normalize (name: string) =
        if name = "HEAD" || (not (isNull name) && name.StartsWith("refs/", StringComparison.Ordinal)) then name
        else "refs/" + name

    let private parseRefValue (content: string) =
        let value = content.Trim()
        if value.StartsWith("ref:", StringComparison.Ordinal) then Symbolic(value.Substring(4).Trim())
        else Direct value

    let private refPath (repo: Repo) (name: string) =
        if name = "HEAD" then Repository.getHeadFile repo
        else Path.Combine(Repository.getCommonDir repo, name.Replace('/', Path.DirectorySeparatorChar))

    let readHead (repo: Repo) : Result<RefValue, string> =
        try
            let path = Repository.getHeadFile repo
            if File.Exists path then Ok (parseRefValue (File.ReadAllText path)) else Error "HEAD file not found"
        with ex -> Error $"Failed to read HEAD: {ex.Message}"

    let private packedLines (repo: Repo) =
        let path = Repository.getPackedRefsFile repo
        if File.Exists path then File.ReadAllLines path else [||]

    let private parsePacked (lines: string[]) =
        lines |> Array.mapi (fun i line -> i + 1, line.TrimEnd('\r')) |> Array.choose (fun (i, line) ->
            if line = "" || line.StartsWith("#") || line.StartsWith("^") then None
            else
                let pos = line.IndexOf(' ')
                if pos <= 0 || pos = line.Length - 1 then raise (InvalidDataException $"Malformed packed ref at line {i}")
                let hash, name = line.Substring(0, pos), line.Substring(pos + 1)
                if not (isObjectId hash && isValidName name) then raise (InvalidDataException $"Invalid packed ref at line {i}")
                Some (name, hash.ToLowerInvariant()))

    let readPackedRefs (repo: Repo) : Result<(string * GitHash)[], string> =
        try Ok (parsePacked (packedLines repo))
        with ex -> Error $"Failed to read packed refs: {ex.Message}"

    let readReference (repo: Repo) (refName: string) : Result<RefValue, string> =
        try
            let name = normalize refName
            if name <> "HEAD" && not (isValidName name) then Error $"Invalid ref name: {refName}"
            else
                let path = refPath repo name
                if File.Exists path then Ok (parseRefValue (File.ReadAllText path))
                elif name = "HEAD" then Error "HEAD file not found"
                else
                    match parsePacked (packedLines repo) |> Array.tryFind (fun (n, _) -> n = name) with
                    | Some (_, hash) -> Ok (Direct hash)
                    | None -> Error $"Reference not found: {name}"
        with ex -> Error $"Failed to read reference {refName}: {ex.Message}"

    let resolveReference (repo: Repo) (refName: string) : Result<GitHash, string> =
        let seen = HashSet<string>(StringComparer.Ordinal)
        let rec resolve name =
            let name = normalize name
            if not (seen.Add name) then Error $"Symbolic ref cycle at {name}"
            else
                match readReference repo name with
                | Ok (Direct hash) when isObjectId hash -> Ok (hash.ToLowerInvariant())
                | Ok (Direct _) -> Error $"Invalid object ID in {name}"
                | Ok (Symbolic target) when isValidName target -> resolve target
                | Ok (Symbolic _) -> Error $"Invalid symbolic target in {name}"
                | Error e -> Error e
        resolve refName

    let private looseRefs (repo: Repo) =
        let dir = Repository.getRefsDir repo
        if not (Directory.Exists dir) then [||]
        else
            Directory.EnumerateFiles(dir, "*", SearchOption.AllDirectories)
            |> Seq.choose (fun path ->
                let name = "refs/" + Path.GetRelativePath(dir, path).Replace(Path.DirectorySeparatorChar, '/')
                if name.EndsWith(".lock", StringComparison.Ordinal) || Path.GetFileName(path).Contains(".lock-") then None
                else
                    if not (isValidName name) then raise (InvalidDataException $"Invalid loose ref: {name}")
                    Some(name, path))
            |> Seq.toArray

    let private allNames (repo: Repo) =
        let names = HashSet<string>(StringComparer.Ordinal)
        for name, _ in parsePacked (packedLines repo) do
            if not (names.Add name) then raise (InvalidDataException $"Duplicate packed ref: {name}")
        for name, _ in looseRefs repo do names.Add name |> ignore
        names |> Seq.sort |> Seq.toArray

    let private listUnder repo (prefix: string) =
        try
            allNames repo |> Array.choose (fun name -> if name.StartsWith(prefix, StringComparison.Ordinal) then Some(name.Substring(prefix.Length)) else None) |> Ok
        with ex -> Error $"Failed to list refs: {ex.Message}"

    let listBranches repo = listUnder repo "refs/heads/"
    let listTags repo = listUnder repo "refs/tags/"
    let listRemotes repo =
        listUnder repo "refs/remotes/"
        |> Result.map (Array.choose (fun name -> let i = name.IndexOf('/') in if i > 0 then Some(name.Substring(0, i)) else None) >> Array.distinct)

    let private includesRef policy (name: string) =
        match policy with
        | RefPolicy.Public -> name.StartsWith("refs/heads/", StringComparison.Ordinal) || name.StartsWith("refs/tags/", StringComparison.Ordinal)
        | RefPolicy.Replication -> name.StartsWith("refs/", StringComparison.Ordinal)

    let snapshot (repo: Repo) (policy: RefPolicy) : Result<RefSnapshot, string> =
        try
            let values = Dictionary<string, RefValue>(StringComparer.Ordinal)
            for name, hash in parsePacked (packedLines repo) do
                if not (values.TryAdd(name, Direct hash)) then
                    raise (InvalidDataException $"Duplicate packed ref: {name}")
            for name, path in looseRefs repo do
                let refValue = parseRefValue (File.ReadAllText path)
                match refValue with
                | Direct hash when isObjectId hash -> values.[name] <- Direct(hash.ToLowerInvariant())
                | Symbolic target when isValidName target -> values.[name] <- refValue
                | _ -> raise (InvalidDataException $"Invalid loose ref: {name}")
            let resolved = Dictionary<string, GitHash>(StringComparer.Ordinal)
            let active = HashSet<string>(StringComparer.Ordinal)
            let rec resolve name =
                match resolved.TryGetValue name with
                | true, hash -> hash
                | false, _ ->
                    if not (active.Add name) then raise (InvalidDataException $"Symbolic ref cycle at {name}")
                    let hash =
                        match values.TryGetValue name with
                        | true, Direct hash -> hash
                        | true, Symbolic target -> resolve target
                        | false, _ -> raise (InvalidDataException $"Symbolic ref target not found: {name}")
                    active.Remove name |> ignore
                    resolved.[name] <- hash
                    hash
            let refs =
                values.Keys
                |> Seq.filter (includesRef policy)
                |> Seq.map (fun name -> name, resolve name)
                |> Map.ofSeq
            let head =
                match readHead repo with
                | Ok (Direct hash) when isObjectId hash -> Direct(hash.ToLowerInvariant())
                | Ok (Symbolic target) when isValidName target -> Symbolic target
                | Ok _ -> raise (InvalidDataException "Invalid HEAD")
                | Error e -> raise (InvalidDataException e)
            let canonical = StringBuilder("fsharpgit-ref-snapshot-v1\n")
            match head with
            | Direct hash -> canonical.Append("HEAD\000").Append(hash).Append('\n') |> ignore
            | Symbolic target -> canonical.Append("HEAD\000ref:").Append(target).Append('\n') |> ignore
            for KeyValue(name, hash) in refs do canonical.Append(name).Append('\000').Append(hash).Append('\n') |> ignore
            let digest = canonical.ToString() |> Encoding.UTF8.GetBytes |> SHA256.HashData |> Convert.ToHexString |> fun s -> s.ToLowerInvariant()
            Ok { Head = head; Refs = refs; Digest = digest }
        with ex -> Error $"Failed to snapshot refs: {ex.Message}"

    // Reject symlinks at every existing path component before creating directories or locks.
    let private safePath (root: string) (path: string) =
        let fullRoot = Path.GetFullPath root
        let fullPath = Path.GetFullPath path
        let relative = Path.GetRelativePath(fullRoot, fullPath)
        if Path.IsPathRooted relative || relative = ".." || relative.StartsWith(".." + string Path.DirectorySeparatorChar) then
            raise (InvalidDataException "Ref path escapes repository")
        let mutable current = fullRoot
        for part in relative.Split(Path.DirectorySeparatorChar) do
            current <- Path.Combine(current, part)
            try
                if (File.GetAttributes current &&& FileAttributes.ReparsePoint) <> enum<FileAttributes> 0 then
                    raise (InvalidDataException $"Symlink in ref path: {current}")
            with
            | :? FileNotFoundException -> ()
            | :? DirectoryNotFoundException -> ()

    let private withRefLock (repo: Repo) name action =
        let path = refPath repo name
        let root = if name = "HEAD" then repo.GitDir else Repository.getCommonDir repo
        safePath root path
        Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
        safePath root path
        let lockPath = path + ".lock"
        safePath root lockPath
        use stream = new FileStream(lockPath, FileMode.CreateNew, FileAccess.Write, FileShare.None)
        try action path lockPath stream
        finally
            stream.Dispose()
            if File.Exists lockPath then File.Delete lockPath

    let private writeLock (stream: FileStream) (text: string) =
        let bytes = Encoding.UTF8.GetBytes(text)
        stream.Write(bytes, 0, bytes.Length)
        stream.Flush(true)
        stream.Dispose()

    let private update (repo: Repo) name expected desired checkExpected =
        try
            if not (isValidName name) then Error $"Invalid ref name: {name}"
            elif expected |> Option.exists (isObjectId >> not) then Error "Invalid expected object ID"
            elif desired |> Option.exists (isObjectId >> not) then Error "Invalid desired object ID"
            else
                withRefLock repo name (fun path lockPath stream ->
                    let packedPath = Repository.getPackedRefsFile repo
                    let current () =
                        if File.Exists path then
                            match parseRefValue (File.ReadAllText path) with
                            | Direct hash when isObjectId hash -> Some(hash.ToLowerInvariant())
                            | _ -> raise (InvalidDataException $"Ref is symbolic or invalid: {name}")
                        else parsePacked (packedLines repo) |> Array.tryPick (fun (n, h) -> if n = name then Some h else None)
                    let check () =
                        let actual = current ()
                        if checkExpected && actual <> Option.map (fun (x: string) -> x.ToLowerInvariant()) expected then
                            raise (InvalidOperationException $"Reference {name} changed")
                    match desired with
                    | Some hash ->
                        check ()
                        writeLock stream (hash.ToLowerInvariant() + "\n")
                        File.Move(lockPath, path, true)
                    | None ->
                        safePath (Repository.getCommonDir repo) packedPath
                        let packedLock = packedPath + ".lock"
                        safePath (Repository.getCommonDir repo) packedLock
                        use ps = new FileStream(packedLock, FileMode.CreateNew, FileAccess.Write, FileShare.None)
                        try
                            check ()
                            let lines = packedLines repo
                            parsePacked lines |> ignore
                            let output = ResizeArray<string>()
                            let mutable skipPeeled = false
                            for line in lines do
                                if line.StartsWith("^") then
                                    if not skipPeeled then output.Add line
                                    skipPeeled <- false
                                else
                                    let separator = line.IndexOf(' ')
                                    skipPeeled <- separator > 0 && line.Substring(separator + 1).TrimEnd('\r') = name
                                    if not skipPeeled then output.Add line
                            if File.Exists packedPath then
                                writeLock ps (String.concat "\n" output + "\n")
                                File.Move(packedLock, packedPath, true)
                            if File.Exists path then File.Delete path
                        finally
                            ps.Dispose()
                            if File.Exists packedLock then File.Delete packedLock
                    Ok ())
        with ex -> Error $"Failed to update ref {name}: {ex.Message}"

    let compareAndSwap (repo: Repo) (name: string) (expected: GitHash option) (desired: GitHash option) : Result<unit, string> =
        update repo name expected desired true
    let writeDirectAtomic repo name hash = update repo name None (Some hash) false
    let deleteAtomic repo name = update repo name None None false

    let findRef repo hash : Result<string option, string> =
        try
            let mutable found = None
            for name in allNames repo do
                match resolveReference repo name with
                | Error e -> raise (InvalidDataException e)
                | Ok value when found.IsNone && String.Equals(value, hash, StringComparison.OrdinalIgnoreCase) -> found <- Some name
                | _ -> ()
            Ok found
        with ex -> Error $"Failed to find reference: {ex.Message}"

    let createBranch repo name startPoint = compareAndSwap repo ("refs/heads/" + name) None (Some startPoint)
    let deleteBranch repo name =
        let full = "refs/heads/" + name
        match resolveReference repo full with
        | Ok hash -> compareAndSwap repo full (Some hash) None
        | Error e -> Error e
    let updateBranch repo name hash = writeDirectAtomic repo ("refs/heads/" + name) hash
    let currentBranch repo =
        match readHead repo with
        | Ok (Symbolic name) when name.StartsWith("refs/heads/", StringComparison.Ordinal) -> Ok (Some(name.Substring(11)))
        | Ok _ -> Ok None
        | Error e -> Error e
    let updateHead (repo: Repo) (value: RefValue) : Result<unit, string> =
        try
            let content =
                match value with
                | Direct hash when isObjectId hash -> hash.ToLowerInvariant() + "\n"
                | Symbolic name when isValidName name -> "ref: " + name + "\n"
                | _ -> raise (InvalidDataException "Invalid HEAD value")
            withRefLock repo "HEAD" (fun path lockPath stream ->
                writeLock stream content
                File.Move(lockPath, path, true)
                Ok ())
        with ex -> Error $"Failed to update HEAD: {ex.Message}"
