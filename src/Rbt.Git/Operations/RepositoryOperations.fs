namespace Rbt.Git

open System
open System.IO
open System.Text
open System.Collections.Generic
open System.Text.RegularExpressions

module RepositoryOperations =
    let private value = function Ok x -> x | Error e -> raise (InvalidDataException e)

    let private headEntries (repo: Repo) =
        let head = References.readHead repo |> value
        let hash =
            match head with
            | Direct h -> Some h
            | Symbolic name ->
                let snapshot = References.snapshot repo RefPolicy.Replication |> value
                Map.tryFind name snapshot.Refs
        match hash with
        | None -> [||]
        | Some h ->
            let commit = ReadObjects.readCommit repo h |> value
            TreeOperations.listTreeEntriesRecursive repo commit.Tree |> value

    let private workEntry (repo: Repo) (honorFileMode: bool) (entry: IndexEntry) =
        let path = TreeOperations.safeWorkTreePath repo entry.Path |> value
        let info = FileInfo path
        if not (isNull info.LinkTarget) then
            Some (Hashing.hashBlob (Encoding.UTF8.GetBytes info.LinkTarget), 0o120000)
        elif entry.Mode &&& 0o170000 = 0o160000 then
            if Directory.Exists path then Some (entry.Hash, entry.Mode) else None
        elif File.Exists path then
            let mode =
                if OperatingSystem.IsWindows() || not honorFileMode then entry.Mode
                elif File.GetUnixFileMode(path) &&& UnixFileMode.UserExecute <> enum<UnixFileMode>(0) then 0o100755
                else 0o100644
            Some (Hashing.hashBlob (File.ReadAllBytes path), mode)
        else None

    type private IgnoreRule = {
        Base: string
        Pattern: Regex
        Basename: bool
        DirectoryOnly: bool
        Negated: bool
    }

    let private ignoreRegex (pattern: string) =
        let output = StringBuilder("^")
        let mutable i = 0
        while i < pattern.Length do
            match pattern.[i] with
            | '\\' when i + 1 < pattern.Length ->
                i <- i + 1
                output.Append(Regex.Escape(string pattern.[i])) |> ignore
            | '*' ->
                let start = i
                while i + 1 < pattern.Length && pattern.[i + 1] = '*' do i <- i + 1
                let doubleStar = i > start
                let atBoundary = start = 0 || pattern.[start - 1] = '/'
                if doubleStar && atBoundary && i + 1 < pattern.Length && pattern.[i + 1] = '/' then
                    output.Append("(?:.*/)?") |> ignore
                    i <- i + 1
                elif doubleStar && atBoundary && i = pattern.Length - 1 then output.Append(".*") |> ignore
                else output.Append("[^/]*") |> ignore
            | '?' -> output.Append("[^/]") |> ignore
            | '[' ->
                let closing = pattern.IndexOf(']', i + 1)
                if closing < 0 then output.Append("\\[") |> ignore
                else
                    let content = pattern.Substring(i + 1, closing - i - 1)
                    if content = "" then output.Append("\\[\\]") |> ignore
                    else
                        let content = if content.[0] = '!' then "^" + content.Substring(1) else content
                        output.Append("(?!/)[") |> ignore
                        output.Append(content.Replace("\\", "\\\\")) |> ignore
                        output.Append(']') |> ignore
                    i <- closing
            | c -> output.Append(Regex.Escape(string c)) |> ignore
            i <- i + 1
        output.Append('$') |> ignore
        try Regex(output.ToString(), RegexOptions.CultureInvariant, TimeSpan.FromSeconds 1.0)
        with :? ArgumentException -> Regex("^" + Regex.Escape(pattern) + "$", RegexOptions.CultureInvariant, TimeSpan.FromSeconds 1.0)

    let private readIgnoreRules (basePath: string) (file: string) =
        if not (File.Exists file) || not (isNull (FileInfo(file).LinkTarget)) then [||]
        else
            File.ReadAllLines file
            |> Array.choose (fun raw ->
                let mutable line = raw.TrimEnd('\r')
                let mutable trimming = true
                while trimming && line.EndsWith(" ", StringComparison.Ordinal) do
                    let mutable slashCount = 0
                    let mutable pos = line.Length - 2
                    while pos >= 0 && line.[pos] = '\\' do
                        slashCount <- slashCount + 1
                        pos <- pos - 1
                    if slashCount % 2 = 0 then line <- line.Substring(0, line.Length - 1)
                    else trimming <- false
                if line = "" || line.StartsWith("#", StringComparison.Ordinal) then None
                else
                    let negated = line.StartsWith("!", StringComparison.Ordinal)
                    if negated then line <- line.Substring 1
                    let directoryOnly = line.EndsWith("/", StringComparison.Ordinal)
                    if directoryOnly then line <- line.Substring(0, line.Length - 1)
                    let anchored = line.StartsWith("/", StringComparison.Ordinal)
                    if anchored then line <- line.Substring 1
                    if line = "" then None
                    else Some {
                        Base = basePath
                        Pattern = ignoreRegex line
                        Basename = not anchored && not (line.Contains('/'))
                        DirectoryOnly = directoryOnly
                        Negated = negated
                    })

    let private untrackedPaths (repo: Repo) (tracked: Set<string>) (includeIgnored: bool) =
        let result = ResizeArray<string>()
        let root = repo.WorkTree.Value
        let initialRules =
            if includeIgnored then [||]
            else
                let home = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
                let xdg =
                    match Environment.GetEnvironmentVariable "XDG_CONFIG_HOME" with
                    | null | "" -> Path.Combine(home, ".config")
                    | path -> path
                let excludes =
                    match TreeOperations.coreSetting repo "excludesfile" |> value with
                    | Some path when path = "~" -> home
                    | Some path when path.StartsWith("~/", StringComparison.Ordinal) -> Path.Combine(home, path.Substring 2)
                    | Some path when not (Path.IsPathRooted path) -> Path.Combine(root, path)
                    | Some path -> path
                    | None -> Path.Combine(xdg, "git", "ignore")
                Array.append
                    (readIgnoreRules "" excludes)
                    (readIgnoreRules "" (Path.Combine(Repository.getCommonDir repo, "info", "exclude")))
        let ignored (rules: IgnoreRule[]) (relative: string) isDirectory =
            let mutable result = false
            for rule in rules do
                if not rule.DirectoryOnly || isDirectory then
                    let local =
                        if rule.Base = "" then Some relative
                        elif relative.StartsWith(rule.Base + "/", StringComparison.Ordinal) then Some (relative.Substring(rule.Base.Length + 1))
                        else None
                    match local with
                    | Some path ->
                        let candidate = if rule.Basename then Path.GetFileName path else path
                        if rule.Pattern.IsMatch candidate then result <- not rule.Negated
                    | None -> ()
            result
        let rec visit (dir: string) (prefix: string) (inherited: IgnoreRule[]) =
            let rules =
                if includeIgnored then inherited
                else Array.append inherited (readIgnoreRules prefix (Path.Combine(dir, ".gitignore")))
            for path in Directory.EnumerateFileSystemEntries dir do
                let name = Path.GetFileName path
                let relative = if prefix = "" then name else prefix + "/" + name
                if not (prefix = "" && name.Equals(".git", StringComparison.OrdinalIgnoreCase)) then
                    let info = FileInfo path
                    let isDirectory = Directory.Exists path && isNull info.LinkTarget
                    if not (ignored rules relative isDirectory) then
                        if isDirectory then
                            if not (tracked.Contains relative) then visit path relative rules
                        elif not (tracked.Contains relative) then result.Add relative
        visit root "" initialRules
        result.ToArray()

    let getStatus (repo: Repo) : Result<StatusEntry[], string> =
        if not (Repository.hasWorkTree repo) then Ok [||]
        else
            try
                let index = IndexParser.readIndex repo |> value
                let honorFileMode = TreeOperations.honorsFileMode repo |> value
                let heads = headEntries repo |> Array.map (fun e -> e.Path, e) |> Map.ofArray
                let tracked = index.Entries |> Array.map (fun e -> e.Path) |> Set.ofArray
                let entries = ResizeArray<StatusEntry>()
                for entry in index.Entries do
                    if entry.Stage <> 0 then raise (InvalidDataException $"Unmerged index entry: {entry.Path}")
                    let staged =
                        match Map.tryFind entry.Path heads with
                        | None -> FileStatus.Added
                        | Some old when old.Hash <> entry.Hash || old.Mode <> entry.Mode -> FileStatus.Modified
                        | _ -> FileStatus.Unchanged
                    let work =
                        match workEntry repo honorFileMode entry with
                        | None -> FileStatus.Deleted
                        | Some (hash, mode) when hash <> entry.Hash || mode <> entry.Mode -> FileStatus.Modified
                        | _ -> FileStatus.Unchanged
                    if staged <> FileStatus.Unchanged || work <> FileStatus.Unchanged then
                        entries.Add { Path = entry.Path; IndexStatus = staged; WorkTreeStatus = work }
                for KeyValue(path, _) in heads do
                    if not (tracked.Contains path) then
                        entries.Add { Path = path; IndexStatus = FileStatus.Deleted; WorkTreeStatus = FileStatus.Unchanged }
                for path in untrackedPaths repo tracked false do
                    entries.Add { Path = path; IndexStatus = FileStatus.Unchanged; WorkTreeStatus = FileStatus.Added }
                Ok (entries.ToArray() |> Array.sortBy (fun e -> e.Path))
            with ex -> Error $"Failed to get status: {ex.Message}"

    let checkout (repo: Repo) (refName: string) : Result<unit, string> =
        if not (Repository.hasWorkTree repo) then Error "Cannot checkout in bare repository"
        else
            try
                let fullRef = if refName.StartsWith("refs/", StringComparison.Ordinal) then refName else "refs/heads/" + refName
                let branch = References.isValidName fullRef && fullRef.StartsWith("refs/heads/", StringComparison.Ordinal)
                let branchHash = if branch then References.resolveReference repo fullRef |> Result.toOption else None
                let rawHash =
                    match branchHash with
                    | Some hash -> hash
                    | None when refName.Length = 40 && (refName |> Seq.forall Uri.IsHexDigit) -> refName.ToLowerInvariant()
                    | None ->
                        match References.resolveReference repo refName with
                        | Ok hash -> hash
                        | Error _ when not (refName.StartsWith("refs/", StringComparison.Ordinal)) ->
                            References.resolveReference repo ("refs/tags/" + refName) |> value
                        | Error e -> raise (InvalidDataException e)
                let rec peel (seen: Set<string>) hash =
                    if seen.Contains hash then raise (InvalidDataException "Tag cycle")
                    match ReadObjects.readObject repo hash |> value with
                    | Commit commit -> hash, commit
                    | Tag tag -> peel (seen.Add hash) tag.Object
                    | _ -> raise (InvalidDataException "Checkout target is not a commit")
                let commitHash, target = peel Set.empty rawHash
                let headValue = if branchHash.IsSome then Symbolic fullRef else Direct commitHash
                let desired = TreeOperations.listTreeEntriesRecursive repo target.Tree |> value
                let index = IndexParser.readIndex repo |> value
                let statuses = getStatus repo |> value
                let desiredMap = desired |> Array.map (fun e -> e.Path, e) |> Map.ofArray
                let indexMap = index.Entries |> Array.map (fun e -> e.Path, e) |> Map.ofArray
                let changed =
                    Set.union (desiredMap |> Map.keys |> Set.ofSeq) (indexMap |> Map.keys |> Set.ofSeq)
                    |> Set.filter (fun p ->
                        match Map.tryFind p indexMap, Map.tryFind p desiredMap with
                        | Some a, Some b -> a.Hash <> b.Hash || a.Mode <> b.Mode
                        | None, None -> false
                        | _ -> true)
                let overlaps (a: string) (b: string) = a = b || a.StartsWith(b + "/", StringComparison.Ordinal) || b.StartsWith(a + "/", StringComparison.Ordinal)
                for status in statuses do
                    if changed |> Seq.exists (overlaps status.Path) then
                        raise (InvalidOperationException $"Local changes would be overwritten: {status.Path}")
                let tracked = index.Entries |> Array.map (fun e -> e.Path) |> Set.ofArray
                for path in untrackedPaths repo tracked true do
                    if changed |> Seq.exists (overlaps path) then
                        raise (InvalidOperationException $"Untracked path would be overwritten: {path}")
                let data = Dictionary<string, byte[]>(StringComparer.Ordinal)
                for entry in desired do
                    // Validate all paths, even unchanged entries.
                    TreeOperations.safeWorkTreePath repo entry.Path |> value |> ignore
                    if changed.Contains entry.Path then
                        if entry.Mode <> 0o100644 && entry.Mode <> 0o100755 && entry.Mode <> 0o120000 && entry.Mode <> 0o160000 then
                            raise (InvalidDataException $"Unsupported tree mode: {entry.Mode}")
                        if entry.Mode <> 0o160000 then data.Add(entry.Path, ReadObjects.readBlob repo entry.Hash |> value)
                for entry in index.Entries do
                    if changed.Contains entry.Path && entry.Mode &&& 0o170000 = 0o160000 then
                        let path = TreeOperations.safeWorkTreePath repo entry.Path |> value
                        if Directory.Exists path && not (Seq.isEmpty (Directory.EnumerateFileSystemEntries path)) then
                            raise (InvalidOperationException $"Cannot overwrite populated gitlink: {entry.Path}")
                // Inspect directory obstructions without following symlinks.
                let rec inspectDirectory (dir: string) =
                    for child in Directory.EnumerateFileSystemEntries dir do
                        let rel = Path.GetRelativePath(repo.WorkTree.Value, child).Replace(Path.DirectorySeparatorChar, '/')
                        let link = (FileInfo child).LinkTarget
                        if isNull link && Directory.Exists child then inspectDirectory child
                        elif not (indexMap.ContainsKey rel && changed.Contains rel) then
                            raise (InvalidOperationException $"Untracked path would be overwritten: {rel}")
                for entry in desired do
                    if changed.Contains entry.Path then
                        let path = TreeOperations.safeWorkTreePath repo entry.Path |> value
                        if Directory.Exists path && isNull (FileInfo(path).LinkTarget) then inspectDirectory path
                for entry in index.Entries |> Array.sortByDescending (fun e -> e.Path.Length) do
                    if changed.Contains entry.Path then
                        let path = TreeOperations.safeWorkTreePath repo entry.Path |> value
                        if File.Exists path || not (isNull (FileInfo(path).LinkTarget)) then File.Delete path
                        elif entry.Mode &&& 0o170000 = 0o160000 && Directory.Exists path then Directory.Delete(path, false)
                let rec removeEmptyDirectories (dir: string) =
                    for child in Directory.EnumerateDirectories dir do
                        if not (isNull (FileInfo(child).LinkTarget)) then
                            raise (InvalidOperationException $"Symlink obstructs checkout: {child}")
                        removeEmptyDirectories child
                    Directory.Delete(dir, false)
                for entry in desired do
                    if changed.Contains entry.Path then
                        let path = TreeOperations.safeWorkTreePath repo entry.Path |> value
                        if Directory.Exists path then removeEmptyDirectories path
                        Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
                        if entry.Mode = 0o160000 then
                            Directory.CreateDirectory path |> ignore
                        elif entry.Mode = 0o120000 then
                            File.CreateSymbolicLink(path, Encoding.UTF8.GetString data.[entry.Path]) |> ignore
                        else
                            File.WriteAllBytes(path, data.[entry.Path])
                            if not (OperatingSystem.IsWindows()) then
                                let mode = if entry.Mode = 0o100755 then 0o755 else 0o644
                                File.SetUnixFileMode(path, enum<UnixFileMode> mode)
                let entries = desired |> Array.map (fun entry ->
                    { Path = entry.Path; Hash = entry.Hash; Mode = entry.Mode; ModifiedTime = DateTimeOffset.UnixEpoch; Size = 0L; Stage = 0 })
                IndexParser.writeIndex repo { Version = 2; Entries = entries } |> value
                References.updateHead repo headValue |> value
                Ok ()
            with ex -> Error $"Checkout failed: {ex.Message}"

    let currentBranch (repo: Repo) = References.currentBranch repo
    let createBranch (repo: Repo) (name: string) (startPoint: GitHash) = References.createBranch repo name startPoint
    let deleteBranch (repo: Repo) (name: string) = References.deleteBranch repo name
    let listBranches (repo: Repo) = References.listBranches repo
    let listTags (repo: Repo) = References.listTags repo
    let getHead (repo: Repo) : Result<GitHash, string> =
        References.readHead repo |> Result.bind (function Direct hash -> Ok hash | Symbolic name -> References.resolveReference repo name)
    let getCommit (repo: Repo) (hash: GitHash) = ReadObjects.readCommit repo hash
    let getLog (repo: Repo) (maxCount: int option) : Result<CommitData[], string> =
        getHead repo |> Result.bind (fun hash ->
            try
                let commits = CommitHistory.walkCommits repo hash |> Seq.map fst
                let selected = match maxCount with Some n -> commits |> Seq.truncate (max 0 n) | None -> commits
                Ok (selected |> Seq.toArray)
            with ex -> Error $"Failed to read log: {ex.Message}")
    let getWorkingTreeChanges repo = getStatus repo |> Result.map (Array.filter (fun e -> e.WorkTreeStatus <> FileStatus.Unchanged))
    let getStagedChanges repo = getStatus repo |> Result.map (Array.filter (fun e -> e.IndexStatus <> FileStatus.Unchanged))
    let isClean repo = getStatus repo |> Result.map Array.isEmpty
    let getFileContent repo path = getHead repo |> Result.bind (fun hash -> TreeOperations.getFileAtCommit repo hash path)
    let listDirectory repo path =
        getHead repo |> Result.bind (fun hash -> ReadObjects.readCommit repo hash |> Result.bind (fun c -> TreeOperations.getTreeAtPath repo c.Tree path))

    let blame (repo: Repo) (path: string) : Result<(GitHash * CommitData)[], string> =
        try
            let head = getHead repo |> value
            let readLines hash =
                let commit = ReadObjects.readCommit repo hash |> value
                let entries = TreeOperations.listTreeEntriesRecursive repo commit.Tree |> value
                match entries |> Array.tryFind (fun e -> e.Path = path) with
                | None -> commit, None
                | Some entry ->
                    let bytes = ReadObjects.readBlob repo entry.Hash |> value
                    let text = Encoding.Latin1.GetString bytes
                    let lines =
                        if bytes.Length = 0 then [||]
                        else
                            let parts = text.Split('\n')
                            let complete = parts.[0 .. parts.Length - 2] |> Array.map (fun line -> line + "\n")
                            if text.EndsWith("\n", StringComparison.Ordinal) then complete
                            else Array.append complete [| parts.[parts.Length - 1] |]
                    commit, Some lines
            let firstCommit, firstLines = readLines head
            let lines = match firstLines with Some l -> l | None -> raise (FileNotFoundException path)
            let result = Array.create lines.Length (head, firstCommit)
            let pending = Queue<CommitData * string[] * int[]>()
            pending.Enqueue(firstCommit, lines, Array.init lines.Length id)
            while pending.Count > 0 do
                let current, currentLines, mapping = pending.Dequeue()
                for parentHash in current.Parents do
                    let parent, parentLines = readLines parentHash
                    match parentLines with
                    | None -> ()
                    | Some previous ->
                        let next = Array.create previous.Length (-1)
                        for i, j in Merge3.lineMatches currentLines previous do
                            if mapping.[i] >= 0 then
                                next.[j] <- mapping.[i]
                                result.[mapping.[i]] <- parentHash, parent
                                mapping.[i] <- -1
                        if next |> Array.exists ((<=) 0) then pending.Enqueue(parent, previous, next)
            Ok result
        with ex -> Error $"Blame failed: {ex.Message}"
