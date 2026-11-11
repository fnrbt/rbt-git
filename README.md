# Rbt.Git

An F# Git plumbing library for local repositories and selected Git HTTP operations.

## Features

- **Read existing Git repositories** - Parse and read from .git directory structure
- **Full object model** - Support for blobs, trees, commits, and tags
- **Commit history operations** - Walk commits, check ancestry, find merge bases
- **Tree operations** - Traverse file trees, resolve paths, list directories
- **Branch and tag management** - List, create, delete branches and tags
- **Status and diff** - Check working tree status, compare commits/trees
- **Merge operations** - 3-way merge, fast-forward, conflict detection
- **HTTP remote operations** - Query and transfer refs over supported smart HTTP(S) endpoints
- **Configuration management** - Parse and update repository Git configuration
- **Typed APIs** - Discriminated unions, records, and `Result`-returning operations

## Requirements

- .NET 10 SDK (including F# compiler)
- Git executable on `PATH` for the regression harness and release action;
  the library itself does not invoke Git subprocesses at runtime

## Quick Start

```fsharp
open Rbt.Git

match Repository.openRepo "." with
| Ok repo ->
    // HEAD is commonly symbolic (for example, refs/heads/main).
    match RepositoryOperations.getHead repo with
    | Ok hash ->
        match ReadObjects.readCommit repo hash with
        | Ok commit ->
            printfn "Author: %s" commit.Author.Name
            printfn "Message: %s" commit.Message
        | Error msg -> printfn "Error: %s" msg
    | Error msg -> printfn "Error resolving HEAD: %s" msg
| Error msg -> printfn "Error opening repository: %s" msg
```

An unborn branch has no commit to resolve; `getHead` returns an error in that case.

## Frontend

Repository namespace mapping and the custom Wisp UI live in the sibling
`../gitfront` repo. This project remains the Git plumbing library.

## Usage Examples

### Opening a Repository

```fsharp
// Open repository at current directory
let repo = Repository.locateRepo "."

// Or use Result type for error handling
match Repository.openRepo "/path/to/repo" with
| Ok repo -> printfn "Opened: %s" repo.Path
| Error msg -> printfn "Failed: %s" msg
```

### Reading Commit History

```fsharp
// Get current HEAD
match RepositoryOperations.getHead repo with
| Ok headHash ->
    // Walk commit history
    for commit, hash in CommitHistory.walkCommits repo headHash do
        printfn "%s %s" hash commit.Message
| Error msg -> printfn "Error: %s" msg
```

### Working with Trees

```fsharp
// Get file content from HEAD
match RepositoryOperations.getFileContent repo "src/Types.fs" with
| Ok content ->
    let text = System.Text.Encoding.UTF8.GetString content
    printfn "%s" text
| Error msg -> printfn "Error: %s" msg

// List files at root
match RepositoryOperations.listDirectory repo "." with
| Ok entries ->
    for entry in entries do
        printfn "%s %s" entry.Path entry.Hash
| Error msg -> printfn "Error: %s" msg
```

### Getting Repository Status

```fsharp
// Check working tree status
match RepositoryOperations.getStatus repo with
| Ok entries ->
    for entry in entries do
        if entry.WorkTreeStatus <> FileStatus.Unchanged then
            printfn "%A %s" entry.WorkTreeStatus entry.Path
| Error msg -> printfn "Error: %s" msg
```

### Branch Operations

```fsharp
// List branches
match RepositoryOperations.listBranches repo with
| Ok branches -> for branch in branches do printfn "%s" branch
| Error msg -> printfn "Error: %s" msg

// Pass a resolved commit hash, not the literal string "HEAD".
match RepositoryOperations.getHead repo with
| Ok hash ->
    match RepositoryOperations.createBranch repo "feature" hash with
    | Ok () -> printfn "Branch created"
    | Error msg -> printfn "Error: %s" msg
| Error msg -> printfn "Error resolving HEAD: %s" msg
```

### Checking Ancestry

```fsharp
// Check if one commit is ancestor of another
match CommitHistory.isAncestor repo commit1 commit2 with
| Ok true -> printfn "commit1 is ancestor of commit2"
| Ok false -> printfn "commit1 is NOT ancestor of commit2"
| Error msg -> printfn "Error: %s" msg

// Find merge base
match CommitHistory.mergeBase repo commit1 commit2 with
| Ok (Some baseHash) -> printfn "Merge base: %s" baseHash
| Ok None -> printfn "No common ancestor"
| Error msg -> printfn "Error: %s" msg
```

### Merging

```fsharp
// These APIs take commit hashes, not branch names. Resolve the branch first.
match RepositoryOperations.getHead repo,
      References.resolveReference repo "refs/heads/feature-branch" with
| Ok ours, Ok theirs ->
    match Merge.canFastForward repo "main" theirs with
    | Ok true ->
        match Merge.fastForward repo "main" theirs with
        | Ok () -> printfn "Branch fast-forwarded"
        | Error msg -> printfn "Fast-forward failed: %s" msg
    | Ok false ->
        match Merge.threeWayMerge repo ours theirs with
        | Ok (Clean treeHash) -> printfn "Persisted merge tree: %s" treeHash
        | Ok (Conflict paths) -> printfn "Merge conflicts: %A" paths
        | Error msg -> printfn "Merge failed: %s" msg
    | Error msg -> printfn "Fast-forward check failed: %s" msg
| Error msg, _ | _, Error msg -> printfn "Cannot resolve branch: %s" msg
```

`threeWayMerge` persists a clean merged tree but does not update a branch,
index, or worktree. `Merge3.merge` creates a persisted merge commit.
Multiple lowest common ancestors require recursive merge-base handling, which
is not supported: `mergeBase` returns an error rather than selecting one.
Unreadable ancestry is also reported as an error.

### Configuration

```fsharp
// Read config value
match GitConfig.getValueFromRepo repo "user" None "name" with
| Ok (Some name) -> printfn "User name: %s" name
| Ok None -> printfn "User name not set"
| Error msg -> printfn "Error: %s" msg

// Set config value
match GitConfig.updateValue repo "user" None "email" "user@example.com" with
| Ok () -> printfn "Email updated"
| Error msg -> printfn "Error: %s" msg

// Read all values in a section
match GitConfig.readConfig repo with
| Ok config ->
    let userValues = GitConfig.getAllValues config "user" None
    for kvp in userValues do
        printfn "%s = %s" kvp.Key kvp.Value
| Error msg -> printfn "Error: %s" msg
```

## Project Structure

The library sources are under `src/Rbt.Git/`, organized into `Core`,
`Parsing`, `Operations`, and `Utilities`. The executable regression harness is
under `tests/Rbt.Git.Tests/`.

## Building

```bash
# Build the library
dotnet build

# Build in release mode
dotnet build -c Release
```

Run the executable regression harness before releasing:

```bash
dotnet run --project tests/Rbt.Git.Tests/Rbt.Git.Tests.fsproj -c Release
```

To run one group while investigating a failure, append its name after `--`
(for example, `dotnet run --project tests/Rbt.Git.Tests -- protocol`).
Groups are `core`, `parsing`, `storage`, `history`, `worktree`, `protocol`,
and `interop`; omitting the group runs all of them.

The Forge tag-publish action runs this harness before packing and uploading;
a failing harness prevents publication.

## Running the Sample

```bash
# Build the Debug assembly referenced by the script, then run from this root.
dotnet build src/Rbt.Git/Rbt.Git.fsproj
dotnet fsi examples/SampleUsage.fsx
```

## API Reference

### Core Modules

#### Rbt.Git.Types
Core types for Git objects, commits, trees, etc.

- `GitHash` - 40-character hex string for object hashes
- `GitObject` - Discriminated union for blob, tree, commit, tag
- `TreeEntry`, `CommitData`, `TagData` - Git object data structures
- `Signature` - Author/committer information
- `Repo`, `PackIndex`, `GitIndex` - Repository structures

#### Rbt.Git.Repository
Repository initialization and path management.

- `locateRepo` - Find repository at or above given path
- `openRepo` - Open repository with Result error handling
- `getCommonDir` - Shared storage directory for linked worktrees
- `getHeadFile`, `getConfigFile`, `getIndexPath` - Path utilities

#### Rbt.Git.GitConfig
Git config file parsing and manipulation.

- `readConfig` - Parse .git/config file
- `writeConfig` - Write config to .git/config
- `getValue`, `getValues` - Last value or all repeated values for a key
- `setValue` - Set specific config value
- `removeValue` - Remove config value
- `removeSection` - Remove entire config section

`ConfigSection.Entries : (string * string)[]` preserves ordered repeated keys.
`ConfigSection.Values : Map<string, string>` remains a last-value view; do not
use it to enumerate repeated settings. `getValues config section subsection key`
returns a `string[]` in configuration order. Code constructing `ConfigSection`
records must populate `Entries`; `Values` is computed and is not a record field.

Worktree status reads `core.filemode` and `core.excludesfile` from direct
system, XDG, user, and repository config files. It does not expand
`include`/`includeIf` or `config.worktree`.

#### Rbt.Git.References
Branch, tag, and reference operations.

- `readHead` - Read HEAD reference
- `readReference` - Read specific reference
- `resolveReference` - Follow symbolic references
- `listBranches`, `listTags` - List references

### Parsing Modules

#### Rbt.Git.ObjectParser
Parse blob, tree, commit, and tag objects.

- `parseObject` - Parse any git object
- `parseBlob`, `parseTree`, `parseCommit`, `parseTag` - Parse specific types

#### Rbt.Git.PackParser
Validate version-2 pack indexes.

- `readPackIndex` - Validate and parse a `.idx` file

Use `ReadObjects.readObject` for parsed objects and
`ReadObjects.readRawObject` for the object type and canonical raw payload.
The legacy direct pack-file reader and delta helpers are no longer public.

#### Rbt.Git.IndexParser
Parse the .git/index file.

- `readIndex` - Parse index to GitIndex structure

### Operations Modules

#### Rbt.Git.ReadObjects
Read git objects from repository.

- `readObject`, `readRawObject`, `readBlob`, `readTree`, `readCommit`, `readTag` - Read objects
- `objectExists` - Check if object exists
- `readObjectCached` - Read with caching

#### Rbt.Git.CommitHistory
Commit walking and ancestry.

- `getParents` - Get commit parents
- `walkCommits` - Walk commit history
- `isAncestor` - Check ancestry
- `mergeBase` - Find common ancestor
- `getRevList` - Get commit hash list

#### Rbt.Git.TreeOperations
Tree traversal and path resolution.

- `resolvePath` - Resolve path in tree
- `listTree`, `listTreeRecursive` - List tree contents
- `getTreeAtCommit` - Get tree from commit
- `getFileAtCommit` - Get file content from commit
- `treeToMap` - Convert tree to path->hash map

#### Rbt.Git.Diff
Compare commits and trees.

- `diffTrees`, `diffCommits` - Compare and return changes
- `getChangedFiles` - Get files changed by commit
- `diffWorkingTree` - Compare indexed worktree paths against a tree

`FileChange.OldMode` and `NewMode` are `int option` values for the modes on
each side of a change, including executable-bit and gitlink changes. An absent
side uses `None`; callers constructing `FileChange` records must populate both.

#### Rbt.Git.Merge
Perform 3-way merges and fast-forward.

- `mergeCommits`, `threeWayMerge` - Persist a clean merged tree or report conflicts
- `canFastForward`, `fastForward` - Check and update a branch
- `Merge3.merge` - Create a persisted merge commit
- `checkConflicts` - Check for conflicts

#### Rbt.Git.Remote
Remote configuration and operations.

- `listRemotes`, `getRemote` - Query remotes
- `addRemote`, `removeRemote` - Modify remotes
- `lsRemote`, `lsRemoteHttp` - List HTTP(S) remote refs
- `fetch` - Fetch HTTP(S) remotes into `refs/remotes/<name>/*`
- `fetchMirror`, `fetchMirrorWithAuth` - Mirror HTTP(S) remotes with optional pruning

#### Rbt.Git.RepositoryOperations
High-level repository operations.

- `getStatus` - Get working tree status
- `checkout` - Checkout reference
- `currentBranch`, `createBranch`, `deleteBranch` - Branch operations
- `getHead`, `getCommit`, `getLog` - Read commit data
- `getFileContent`, `listDirectory` - File operations
- `blame` - Get blame info

### Utilities Modules

#### Rbt.Git.Hashing
SHA-1 hashing functions.

- `sha1`, `sha1String` - Compute SHA-1 hash
- `hashBlob`, `hashTree`, `hashCommit` - Hash git objects
- `parseHash`, `isValidHash` - Hash utilities

#### Rbt.Git.Compression
Zlib compression/decompression.

- `compress`, `decompress` - Compress/decompress byte arrays
- `compressToStream`, `decompressStream` - Stream operations

#### Rbt.Git.PathUtils
Path normalization utilities.

- `normalizeGitPath`, `toGitPath` - Convert to git paths
- `joinPaths`, `splitPath` - Path manipulation
- `relativePath`, `isSubPath` - Path relationships

## Design and compatibility

- Git object parsing and hash operations use typed F# values; repository
  reads, writes, caches, and network transfers have side effects and are not
  pure or globally immutable.
- Most fallible APIs return `Result`, but some sequence operations can raise
  while enumerated. Handle errors and use repository data you trust.
- Git's SHA-1 object format is supported; do not assume SHA-256 repositories,
  every index/pack extension, every transport, or arbitrary Git configuration
  syntax is supported. Remote transfers are HTTP(S), not SSH or local-path
  transport.
- Reference updates use exclusive lock files and compare-and-swap where
  required to protect against cooperating Git writers, not hostile local
  filesystem mutations. Multi-file operations, including checkout, are not
  transactions: unexpected filesystem I/O failures may leave partial changes
  without guaranteed rollback. Handle conflict and I/O errors explicitly.

## License

MIT License

## Contributing

Contributions are welcome! Please ensure:
- Code follows existing patterns
- All public functions have XML documentation comments
- Type annotations are used where needed
- Error handling uses Result types
