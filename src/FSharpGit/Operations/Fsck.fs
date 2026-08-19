namespace FSharpGit

open System
open System.Buffers.Binary
open System.Collections.Generic
open System.IO
open System.Security.Cryptography
open System.Text

module Fsck =
    let private canonicalBytes (objectType: string) (content: byte[]) =
        Array.concat [| Encoding.UTF8.GetBytes($"{objectType} {content.Length}\000"); content |]

    let private canonicalHash objectType content =
        canonicalBytes objectType content |> Hashing.sha1

    let private issue severity kind objectId path message =
        { Severity = severity; Kind = kind; ObjectId = objectId; Path = path; Message = message }

    let private parseRaw objectType content =
        canonicalBytes objectType content |> ObjectParser.parseObject

    let private validateObjectId expected objectType content =
        let actual = canonicalHash objectType content
        if actual = expected then Ok ()
        else Error $"object hashes to {actual}, expected {expected}"

    let private objectChildren (objectValue: GitObject) =
        seq {
            match objectValue with
            | Blob _ -> ()
            | Commit commit ->
                yield commit.Tree, Some "tree"
                for parent in commit.Parents do yield parent, Some "commit"
            | Tree entries ->
                for entry in entries do
                    // Gitlinks name commits in another repository; their objects are
                    // intentionally not required in this object database.
                    if entry.Mode <> 0o160000 then
                        let expectedType = if entry.Mode = 0o040000 then "tree" else "blob"
                        yield entry.Hash, Some expectedType
            | Tag tag -> yield tag.Object, Some tag.ObjectType
        }

    let private verifyReachableCore (repo: Repo) (snapshot: RefSnapshot) =
        let issues = ResizeArray<FsckIssue>()
        let visited = HashSet<GitHash>(StringComparer.Ordinal)
        let pending = Stack<GitHash * string option>()
        for KeyValue(_, hash) in snapshot.Refs do pending.Push(hash, None)
        match snapshot.Head with
        | Direct hash -> pending.Push(hash, None)
        | Symbolic _ -> ()

        while pending.Count > 0 do
            let hash, expectedType = pending.Pop()
            if visited.Add hash then
                match ReadObjects.readRawObject repo hash with
                | Error error ->
                    issues.Add(issue FsckSeverity.Error FsckIssueKind.Object (Some hash) None error)
                | Ok (objectType, content) ->
                    match expectedType with
                    | Some expected when expected <> objectType ->
                        issues.Add(issue FsckSeverity.Error FsckIssueKind.Object (Some hash) None $"expected {expected}, found {objectType}")
                    | _ -> ()
                    match validateObjectId hash objectType content with
                    | Error error -> issues.Add(issue FsckSeverity.Error FsckIssueKind.Object (Some hash) None error)
                    | Ok () -> ()
                    match parseRaw objectType content with
                    | Error error -> issues.Add(issue FsckSeverity.Error FsckIssueKind.Object (Some hash) None error)
                    | Ok objectValue ->
                        for child in objectChildren objectValue do pending.Push child
        visited, issues

    let verifyReachable (repo: Repo) (snapshot: RefSnapshot) : Result<FsckReport, string> =
        try
            let visited, issues = verifyReachableCore repo snapshot
            Ok {
                RefsChecked = snapshot.Refs.Count
                ObjectsChecked = visited.Count
                PacksChecked = 0
                Issues = List.ofSeq issues
            }
        with ex -> Error $"Reachable fsck failed: {ex.Message}"

    /// Verify objects introduced by a receive-pack without rescanning repository
    /// history. Received objects and updated ref tips are parsed completely.
    /// Pre-existing objects reached from them are existence-checked boundaries;
    /// full historical verification belongs to fsck.
    let verifyIntroduced
        (repo: Repo)
        (updatedTips: GitHash seq)
        (introducedObjects: GitHash seq) : Result<FsckReport, string> =
        try
            let introduced = HashSet<GitHash>(introducedObjects, StringComparer.Ordinal)
            let tips = updatedTips |> Seq.distinct |> Array.ofSeq
            let issues = ResizeArray<FsckIssue>()
            let checkedObjects = HashSet<GitHash>(StringComparer.Ordinal)
            let objectTypes = Dictionary<GitHash, string>(StringComparer.Ordinal)
            let pending = Stack<GitHash * string option * bool>()
            for hash in introduced do pending.Push(hash, None, true)
            for hash in tips do pending.Push(hash, None, true)

            let checkExpectedType hash expectedType objectType =
                match expectedType with
                | Some expected when expected <> objectType ->
                    issues.Add(
                        issue
                            FsckSeverity.Error
                            FsckIssueKind.Object
                            (Some hash)
                            None
                            $"expected {expected}, found {objectType}")
                | _ -> ()

            while pending.Count > 0 do
                let hash, expectedType, validate = pending.Pop()
                match objectTypes.TryGetValue hash with
                | true, objectType ->
                    checkExpectedType hash expectedType objectType
                | false, _ when validate ->
                    checkedObjects.Add hash |> ignore
                    match ReadObjects.readRawObject repo hash with
                    | Error error ->
                        issues.Add(issue FsckSeverity.Error FsckIssueKind.Object (Some hash) None error)
                    | Ok (objectType, content) ->
                        objectTypes.[hash] <- objectType
                        checkExpectedType hash expectedType objectType
                        match validateObjectId hash objectType content with
                        | Error error ->
                            issues.Add(issue FsckSeverity.Error FsckIssueKind.Object (Some hash) None error)
                        | Ok () -> ()
                        match parseRaw objectType content with
                        | Error error ->
                            issues.Add(issue FsckSeverity.Error FsckIssueKind.Object (Some hash) None error)
                        | Ok objectValue ->
                            for childHash, childType in objectChildren objectValue do
                                pending.Push(childHash, childType, introduced.Contains childHash)
                | false, _ ->
                    if checkedObjects.Add hash && not (ReadObjects.objectExists repo hash) then
                        issues.Add(
                            issue
                                FsckSeverity.Error
                                FsckIssueKind.Object
                                (Some hash)
                                None
                                $"Object not found: {hash}")

            Ok {
                RefsChecked = tips.Length
                ObjectsChecked = checkedObjects.Count
                PacksChecked = 0
                Issues = List.ofSeq issues
            }
        with ex -> Error $"Introduced-object fsck failed: {ex.Message}"

    let private validateLooseObject (path: string) (expectedHash: GitHash) =
        try
            let raw = File.ReadAllBytes path |> Compression.decompress
            let actualHash = Hashing.sha1 raw
            if actualHash <> expectedHash then Error $"loose object hashes to {actualHash}, expected {expectedHash}"
            else
                let nul = Array.IndexOf(raw, 0uy)
                if nul <= 0 then Error "loose object has no header terminator"
                else
                    let header = Encoding.UTF8.GetString(raw, 0, nul)
                    let separator = header.IndexOf(' ')
                    if separator <= 0 || separator = header.Length - 1 then Error "loose object has malformed header"
                    else
                        let objectType = header.Substring(0, separator)
                        let mutable declaredLength = 0
                        if objectType <> "blob" && objectType <> "tree" && objectType <> "commit" && objectType <> "tag" then
                            Error $"loose object has unsupported type {objectType}"
                        elif not (Int32.TryParse(header.Substring(separator + 1), &declaredLength)) then
                            Error "loose object has invalid declared length"
                        elif declaredLength <> raw.Length - nul - 1 then
                            Error $"loose object declares {declaredLength} bytes but contains {raw.Length - nul - 1}"
                        else
                            match ObjectParser.parseObject raw with
                            | Ok _ -> Ok ()
                            | Error error -> Error error
        with ex -> Error ex.Message

    let private crc32 (bytes: byte[]) offset count =
        let mutable crc = 0xffffffffu
        for index in offset .. offset + count - 1 do
            crc <- crc ^^^ uint32 bytes.[index]
            for _ in 1 .. 8 do
                let mask = 0u - (crc &&& 1u)
                crc <- (crc >>> 1) ^^^ (0xedb88320u &&& mask)
        ~~~crc

    let private bytesEqual (left: byte[]) (right: byte[]) =
        left.Length = right.Length && CryptographicOperations.FixedTimeEquals(left, right)

    let private hashBytes (hash: GitHash) = Convert.FromHexString hash

    let private verifyPackPair (repo: Repo) (idxPath: string) (packPath: string) =
        let issues = ResizeArray<FsckIssue>()
        let add kind objectId path message =
            issues.Add(issue FsckSeverity.Error kind objectId (Some path) message)
        try
            let pack = File.ReadAllBytes packPath
            let indexBytes = File.ReadAllBytes idxPath
            if pack.Length < 32 then
                add FsckIssueKind.Pack None packPath "pack is too short"
            elif not (pack.[0] = byte 'P' && pack.[1] = byte 'A' && pack.[2] = byte 'C' && pack.[3] = byte 'K') then
                add FsckIssueKind.Pack None packPath "invalid PACK signature"
            else
                let version = BinaryPrimitives.ReadUInt32BigEndian(pack.AsSpan(4, 4))
                if version <> 2u && version <> 3u then add FsckIssueKind.Pack None packPath $"unsupported pack version {version}"
                let declaredCount = BinaryPrimitives.ReadUInt32BigEndian(pack.AsSpan(8, 4)) |> int
                let packTrailer = pack.[pack.Length - 20 ..]
                let computedPackHash = SHA1.HashData(pack.AsSpan(0, pack.Length - 20))
                if not (bytesEqual packTrailer computedPackHash) then add FsckIssueKind.Pack None packPath "pack trailer checksum mismatch"

                if indexBytes.Length < 40 then
                    add FsckIssueKind.Index None idxPath "pack index is too short"
                else
                    let indexTrailer = indexBytes.[indexBytes.Length - 20 ..]
                    let computedIndexHash = SHA1.HashData(indexBytes.AsSpan(0, indexBytes.Length - 20))
                    if not (bytesEqual indexTrailer computedIndexHash) then add FsckIssueKind.Index None idxPath "index trailer checksum mismatch"
                    let embeddedPackHash = indexBytes.[indexBytes.Length - 40 .. indexBytes.Length - 21]
                    if not (bytesEqual embeddedPackHash packTrailer) then add FsckIssueKind.Index None idxPath "index embedded pack checksum mismatch"

                match PackParser.readPackIndex idxPath with
                | Error error -> add FsckIssueKind.Index None idxPath error
                | Ok packIndex ->
                    if packIndex.Version <> 2 then add FsckIssueKind.Index None idxPath $"unsupported pack index version {packIndex.Version}"
                    if packIndex.Objects.Length <> declaredCount then
                        add FsckIssueKind.Index None idxPath $"index has {packIndex.Objects.Length} objects; pack declares {declaredCount}"
                    for i in 1 .. 255 do
                        if packIndex.Fanout.[i] < packIndex.Fanout.[i - 1] then
                            add FsckIssueKind.Index None idxPath $"fanout table decreases at bucket {i}"
                    if packIndex.Fanout.[255] <> packIndex.Objects.Length then
                        add FsckIssueKind.Index None idxPath "fanout total does not match object count"
                    for i in 1 .. packIndex.Objects.Length - 1 do
                        if StringComparer.Ordinal.Compare(packIndex.Objects.[i - 1].Hash, packIndex.Objects.[i].Hash) >= 0 then
                            add FsckIssueKind.Index (Some packIndex.Objects.[i].Hash) idxPath "object IDs are not strictly sorted"
                    let expectedFanout = Array.zeroCreate 256
                    for entry in packIndex.Objects do expectedFanout.[int (Convert.ToByte(entry.Hash.Substring(0, 2), 16))] <- expectedFanout.[int (Convert.ToByte(entry.Hash.Substring(0, 2), 16))] + 1
                    for i in 1 .. 255 do expectedFanout.[i] <- expectedFanout.[i] + expectedFanout.[i - 1]
                    if expectedFanout <> packIndex.Fanout then add FsckIssueKind.Index None idxPath "fanout table does not match object IDs"

                    let byOffset = Dictionary<int, FSharpGit.PackObjectEntry>()
                    for entry in packIndex.Objects do
                        if entry.Offset < 12L || entry.Offset >= int64 (pack.Length - 20) then
                            add FsckIssueKind.Index (Some entry.Hash) idxPath $"invalid pack offset {entry.Offset}"
                        elif entry.Offset > int64 Int32.MaxValue then
                            add FsckIssueKind.Index (Some entry.Hash) idxPath "pack offset exceeds supported in-memory reader range"
                        else byOffset.[int entry.Offset] <- entry
                    let ordered = byOffset.Keys |> Seq.sort |> Seq.toArray
                    for i in 0 .. ordered.Length - 1 do
                        let start = ordered.[i]
                        let finish = if i + 1 < ordered.Length then ordered.[i + 1] else pack.Length - 20
                        let entry = byOffset.[start]
                        if crc32 pack start (finish - start) <> entry.CRC then
                            add FsckIssueKind.Index (Some entry.Hash) idxPath "packed object CRC mismatch"

                    let hashOffsets = Dictionary<GitHash, int>(StringComparer.Ordinal)
                    for entry in packIndex.Objects do
                        if entry.Offset <= int64 Int32.MaxValue then hashOffsets.[entry.Hash] <- int entry.Offset
                    let cache = Dictionary<int, string * byte[]>()
                    let active = HashSet<int>()
                    let rec readAt offset =
                        match cache.TryGetValue offset with
                        | true, value -> Some value
                        | _ when not (active.Add offset) -> failwith $"delta cycle at pack offset {offset}"
                        | _ ->
                            let value = PackData.readObjectAt pack offset readAt readHash
                            active.Remove offset |> ignore
                            cache.[offset] <- value
                            Some value
                    and readHash hash =
                        match hashOffsets.TryGetValue hash with
                        | true, offset -> readAt offset
                        | _ -> ReadObjects.readRawObject repo hash |> Result.toOption
                    for entry in packIndex.Objects do
                        if entry.Offset <= int64 Int32.MaxValue then
                            try
                                match readAt (int entry.Offset) with
                                | None -> add FsckIssueKind.Pack (Some entry.Hash) packPath "could not reconstruct packed object"
                                | Some (objectType, content) ->
                                    match validateObjectId entry.Hash objectType content with
                                    | Ok () -> ()
                                    | Error error -> add FsckIssueKind.Pack (Some entry.Hash) packPath error
                                    match parseRaw objectType content with
                                    | Ok _ -> ()
                                    | Error error -> add FsckIssueKind.Pack (Some entry.Hash) packPath error
                            with ex -> add FsckIssueKind.Pack (Some entry.Hash) packPath ex.Message
        with ex -> add FsckIssueKind.Pack None packPath ex.Message
        List.ofSeq issues

    let verifyFull (repo: Repo) : Result<FsckReport, string> =
        try
            match References.snapshot repo RefPolicy.Replication with
            | Error error -> Error error
            | Ok snapshot ->
                let visited, issues = verifyReachableCore repo snapshot
                let allIssues = ResizeArray<FsckIssue>(issues)
                let mutable objectsChecked = visited.Count
                let objectsDir = Repository.getObjectsDir repo
                if Directory.Exists objectsDir then
                    for directory in Directory.EnumerateDirectories objectsDir do
                        let prefix = Path.GetFileName directory
                        if prefix.Length = 2 && prefix |> Seq.forall Uri.IsHexDigit then
                            for path in Directory.EnumerateFiles directory do
                                let name = Path.GetFileName path
                                let hash = (prefix + name).ToLowerInvariant()
                                if name.Length <> 38 || not (hash |> Seq.forall Uri.IsHexDigit) then
                                    allIssues.Add(issue FsckSeverity.Error FsckIssueKind.Object None (Some path) "invalid loose-object path")
                                else
                                    if not (visited.Contains hash) then objectsChecked <- objectsChecked + 1
                                    match validateLooseObject path hash with
                                    | Ok () -> ()
                                    | Error error -> allIssues.Add(issue FsckSeverity.Error FsckIssueKind.Object (Some hash) (Some path) error)

                let packDir = Repository.getPackDir repo
                let mutable packsChecked = 0
                if Directory.Exists packDir then
                    let packNames = Directory.EnumerateFiles(packDir, "*.pack") |> Seq.map Path.GetFileNameWithoutExtension |> Set.ofSeq
                    let indexNames = Directory.EnumerateFiles(packDir, "*.idx") |> Seq.map Path.GetFileNameWithoutExtension |> Set.ofSeq
                    for orphan in Set.difference packNames indexNames do
                        allIssues.Add(issue FsckSeverity.Error FsckIssueKind.Pack None (Some (Repository.getPackFilePath repo orphan)) "pack has no index")
                    for orphan in Set.difference indexNames packNames do
                        allIssues.Add(issue FsckSeverity.Error FsckIssueKind.Index None (Some (Repository.getPackIndexPath repo orphan)) "index has no pack")
                    for name in Set.intersect packNames indexNames do
                        packsChecked <- packsChecked + 1
                        let idxPath = Repository.getPackIndexPath repo name
                        let packPath = Repository.getPackFilePath repo name
                        match PackParser.readPackIndex idxPath with
                        | Ok index ->
                            for entry in index.Objects do
                                if not (visited.Contains entry.Hash) then objectsChecked <- objectsChecked + 1
                        | Error _ -> ()
                        for packIssue in verifyPackPair repo idxPath packPath do allIssues.Add packIssue

                Ok {
                    RefsChecked = snapshot.Refs.Count
                    ObjectsChecked = objectsChecked
                    PacksChecked = packsChecked
                    Issues = List.ofSeq allIssues
                }
        with ex -> Error $"Full fsck failed: {ex.Message}"
