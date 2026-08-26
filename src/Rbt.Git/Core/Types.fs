namespace Rbt.Git

open System

type GitHash = string

type TreeEntry = {
    Mode: int
    Path: string
    Hash: GitHash
}

type Signature = {
    Name: string
    Email: string
    Time: DateTimeOffset
}

type CommitData = {
    Tree: GitHash
    Parents: GitHash[]
    Author: Signature
    Committer: Signature
    Message: string
}

type TagData = {
    Object: GitHash
    ObjectType: string
    Tag: string
    Tagger: Signature
    Message: string
}

type GitObject = 
    | Blob of byte[]
    | Tree of TreeEntry[]
    | Commit of CommitData
    | Tag of TagData

type RefType = 
    | Branch of string
    | TagRef of string
    | Remote of string
    | Head

type RefValue = 
    | Direct of GitHash
    | Symbolic of string

[<RequireQualifiedAccess>]
type RefPolicy =
    | Public
    | Replication

type RefSnapshot = {
    Head: RefValue
    Refs: Map<string, GitHash>
    Digest: string
}

[<RequireQualifiedAccess>]
type FsckSeverity =
    | Error
    | Warning

[<RequireQualifiedAccess>]
type FsckIssueKind =
    | Reference
    | Object
    | Pack
    | Index

type FsckIssue = {
    Severity: FsckSeverity
    Kind: FsckIssueKind
    ObjectId: GitHash option
    Path: string option
    Message: string
}

type FsckReport = {
    RefsChecked: int
    ObjectsChecked: int
    PacksChecked: int
    Issues: FsckIssue list
}
with
    member this.IsValid =
        this.Issues |> List.forall (fun issue -> issue.Severity <> FsckSeverity.Error)

type FileStatus = 
    | Modified
    | Added
    | Deleted
    | Renamed
    | Unchanged

type ChangeType = 
    | Added
    | Deleted
    | Modified
    | Renamed

type StatusEntry = {
    Path: string
    IndexStatus: FileStatus
    WorkTreeStatus: FileStatus
}

type FileChange = {
    Path: string
    OldHash: GitHash option
    NewHash: GitHash option
    ChangeType: ChangeType
}

type MergeResult = 
    | Clean of GitHash
    | Conflict of string[]

type RemoteRepo = {
    Name: string
    FetchUrl: string
    PushUrl: string option
}

type Repo = {
    Path: string
    GitDir: string
    WorkTree: string option
}

type PackIndex = {
    Version: int
    Fanout: int[]
    Objects: PackObjectEntry[]
}

and PackObjectEntry = {
    Hash: GitHash
    Offset: int64
    CRC: uint32
}

type GitIndex = {
    Version: int
    Entries: IndexEntry[]
}

and IndexEntry = {
    Path: string
    Hash: GitHash
    Mode: int
    ModifiedTime: DateTimeOffset
    Size: int64
    Stage: int
}
