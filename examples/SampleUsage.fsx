// Sample usage of Rbt.Git
// Build first: dotnet build
// Then run: dotnet fsi examples/SampleUsage.fsx

#r "../src/Rbt.Git/bin/Debug/net10.0/Rbt.Git.dll"

open Rbt.Git
open System

printfn "Rbt.Git - Sample Usage"
printfn "==============================="

// Try to open a repository in current directory
match Repository.openRepo "." with
| Ok repo ->
    printfn "Repository opened: %s" repo.Path
    printfn "Git directory: %s" repo.GitDir
    match repo.WorkTree with
    | Some wt -> printfn "Working tree: %s" wt
    | None -> printfn "Bare repository"
    
    // Try to get current HEAD
    printfn "\n--- Getting HEAD ---"
    match References.readHead repo with
    | Ok (Direct hash) ->
        printfn "HEAD (direct): %s" hash
        match ReadObjects.readCommit repo hash with
        | Ok commit ->
            printfn "Commit: %s" commit.Message
            printfn "Author: %s <%s>" commit.Author.Name commit.Author.Email
            printfn "Date: %O" commit.Author.Time
        | Error msg -> printfn "Error reading commit: %s" msg
    | Ok (Symbolic refName) ->
        printfn "HEAD (symbolic): %s" refName
        match References.resolveReference repo refName with
        | Ok hash ->
            match ReadObjects.readCommit repo hash with
            | Ok commit ->
                printfn "Commit: %s" commit.Message
                printfn "Author: %s <%s>" commit.Author.Name commit.Author.Email
                printfn "Date: %O" commit.Author.Time
            | Error msg -> printfn "Error reading commit: %s" msg
        | Error msg -> printfn "Error resolving reference: %s" msg
    | Error msg -> printfn "Error reading HEAD: %s" msg
    
    // Try to list branches
    printfn "\n--- Branches ---"
    match References.listBranches repo with
    | Ok branches ->
        if Array.isEmpty branches then
            printfn "No branches found"
        else
            for branch in branches do
                printfn "  - %s" branch
    | Error msg -> printfn "Error listing branches: %s" msg
    
    // Try to list tags
    printfn "\n--- Tags ---"
    match References.listTags repo with
    | Ok tags ->
        if Array.isEmpty tags then
            printfn "No tags found"
        else
            for tag in tags do
                printfn "  - %s" tag
    | Error msg -> printfn "Error listing tags: %s" msg
    
    // Try to get repository status
    printfn "\n--- Status ---"
    match RepositoryOperations.getStatus repo with
    | Ok entries ->
        if Array.isEmpty entries then
            printfn "No changes"
        else
            for entry in entries do
                printfn "index=%A worktree=%A %s" entry.IndexStatus entry.WorkTreeStatus entry.Path
    | Error msg -> printfn "Error getting status: %s" msg
    
    // Try to get log
    printfn "\n--- Recent Commits ---"
    match RepositoryOperations.getHead repo with
    | Ok headHash ->
        for commit, hash in CommitHistory.walkCommits repo headHash |> Seq.truncate 5 do
            printfn "%s %s" hash.[0..6] commit.Message
    | Error msg -> printfn "Error getting log: %s" msg
| Error msg ->
    printfn "Error opening repository: %s" msg
