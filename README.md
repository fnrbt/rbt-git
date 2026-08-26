# Rbt.Git

A pure F# library for reading Git repositories, implemented with zero external dependencies.

## Features

- **Read existing Git repositories** - Parse and read from .git directory structure
- **Full object model** - Support for blobs, trees, commits, and tags
- **Commit history operations** - Walk commits, check ancestry, find merge bases
- **Tree operations** - Traverse file trees, resolve paths, list directories
- **Branch and tag management** - List, create, delete branches and tags
- **Status and diff** - Check working tree status, compare commits/trees
- **Merge operations** - 3-way merge, fast-forward, conflict detection
- **Remote operations** - Parse remote configurations
- **Configuration management** - Principled Git config file parsing and manipulation
- **Pure functional** - Uses discriminated unions, immutable types, Result types for error handling

## Requirements

- .NET 10.0 or later
- F# compiler

## Quick Start

```fsharp
// Open a repository
match Repository.openRepo "." with
| Ok repo ->
    // Get current HEAD
    match References.readHead repo with
    | Ok (Direct hash) ->
        // Read commit data
        match ReadObjects.readCommit repo hash with
        | Ok commit ->
            printfn "Author: %s" commit.Author.Name
            printfn "Message: %s" commit.Message
        | Error msg -> printfn "Error: %s" msg
    | Error msg -> printfn "Error: %s" msg
| Error msg ->
    printfn "Error: %s" msg
```

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
| Ok branches ->
    for branch in branches do
        printfn "%s" branch
| Error msg -> printfn "Error: %s" msg

// Create a new branch
match RepositoryOperations.createBranch repo "feature" "HEAD" with
| Ok () -> printfn "Branch created"
| Error msg -> printfn "Error: %s" msg
```

### Checking Ancestry

```fsharp
// Check if one commit is ancestor of another
match CommitHistory.isAncestor repo commit1 commit2 with
| Ok true -> printfn "commit1 is ancestor of commit2"
| Ok false -> printfn "commit1 is NOT ancestor of commit2"
| Error msg -> printfn "Error: %s" msg

// Find merge base
match CommitHistory.mergeBase repo branch1 branch2 with
| Ok (Some base) -> printfn "Merge base: %s" base
| Ok None -> printfn "No common ancestor"
| Error msg -> printfn "Error: %s" msg
```

### Merging

```fsharp
// Check if fast-forward is possible
match Merge.canFastForward repo "main" "feature-branch" with
| Ok true ->
    // Perform fast-forward
    Merge.fastForward repo "main" "feature-branch"
| Ok false ->
    // Need 3-way merge
    match Merge.threeWayMerge repo "main" "feature-branch" with
    | Ok (Clean _) -> printfn "Merge succeeded"
    | Ok (Conflict paths) -> printfn "Merge conflicts: %A" paths
    | Error msg -> printfn "Error: %s" msg
| Error msg -> printfn "Error: %s" msg
```

### Configuration

```fsharp
// Read config value
match GitConfig.getValueFromRepo repo "user" None "name" with
| Ok (Some name) -> printfn "User name: %s" name
| Ok None -> printfn "User name not set"
| Error msg -> printfn "Error: %s" msg

// Set config value
GitConfig.updateValue repo "user" None "email" "user@example.com" |> ignore

// Read all values in a section
match GitConfig.readConfig repo with
| Ok config ->
    let userValues = GitConfig.getAllValues config "user" None
    for kvp in userValues do
        printfn "%s = %s" kvp.Key kvp.Value
| Error msg -> printfn "Error: %s" msg
```

## Project Structure

```
src/Rbt.Git/
├── Core/
│   ├── Types.fs          - Core discriminated unions and types
│   ├── Repository.fs     - Repository structure and path management
│   ├── GitConfig.fs      - Git config file parsing and manipulation
│   └── References.fs     - Branch, tag, and reference operations
├── Parsing/
│   ├── ObjectParser.fs   - Parse loose git objects
│   ├── PackParser.fs    - Parse pack files and indexes
│   └── IndexParser.fs   - Parse .git/index
├── Operations/
│   ├── ReadObjects.fs    - Unified object reading
│   ├── CommitHistory.fs  - Commit walking and ancestry
│   ├── TreeOperations.fs - Tree traversal and path resolution
│   ├── Diff.fs           - Change detection and diffing
│   ├── Merge.fs          - 3-way merge operations
│   ├── Remote.fs         - Remote operations
│   └── RepositoryOperations.fs - High-level repository operations
└── Utilities/
    ├── Hashing.fs        - SHA-1 utilities
    ├── Compression.fs    - Zlib compression/decompression
    └── PathUtils.fs      - Path utilities
```

## Building

```bash
# Build the library
dotnet build

# Build in release mode
dotnet build -c Release
```

## Running the Sample

```bash
# Make sure you're in a git repository
git init

# Run the sample script
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
- `getHeadFile`, `getConfigFile`, `getIndexPath` - Path utilities

#### Rbt.Git.GitConfig
Git config file parsing and manipulation.

- `readConfig` - Parse .git/config file
- `writeConfig` - Write config to .git/config
- `getValue` - Get specific config value
- `setValue` - Set specific config value
- `removeValue` - Remove config value
- `removeSection` - Remove entire config section

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
Parse pack index and pack files.

- `readPackIndex` - Parse .idx file
- `readPackFile` - Read object from .pack file
- `findPackObject` - Find object in any pack file

#### Rbt.Git.IndexParser
Parse the .git/index file.

- `readIndex` - Parse index to GitIndex structure

### Operations Modules

#### Rbt.Git.ReadObjects
Read git objects from repository.

- `readObject`, `readBlob`, `readTree`, `readCommit`, `readTag` - Read specific objects
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
- `diffWorkingTree` - Diff working tree vs index

#### Rbt.Git.Merge
Perform 3-way merges and fast-forward.

- `mergeCommits` - Merge commits with base
- `canFastForward`, `fastForward` - Fast-forward operations
- `threeWayMerge` - Merge two commits
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

## Design Principles

1. **Pure Functional** - All code uses immutable data structures and pure functions
2. **Result Types** - Operations return `Result<'T, 'TError>` for explicit error handling
3. **Type Safety** - Discriminated unions and records prevent invalid states
4. **Zero Dependencies** - Only .NET 10 BCL, no external packages
5. **Principled Configuration** - Structured API for Git config manipulation

## License

MIT License

## Contributing

Contributions are welcome! Please ensure:
- Code follows existing patterns
- All public functions have XML documentation comments
- Type annotations are used where needed
- Error handling uses Result types
