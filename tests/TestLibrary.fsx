#r "../src/FSharpGit/bin/Release/net10.0/FSharpGit.dll"

open FSharpGit
open System

let testRepositoryOpening repoPath =
    printfn "\n=== Testing Repository Opening ==="
    match Repository.openRepo repoPath with
    | Ok repo ->
        printfn "✓ Repository opened successfully"
        printfn "  Path: %s" repo.Path
        printfn "  GitDir: %s" repo.GitDir
        match repo.WorkTree with
        | Some wt -> printfn "  WorkTree: %s" wt
        | None -> printfn "  WorkTree: None (bare repo)"
        true
    | Error msg ->
        printfn "✗ Failed to open repository: %s" msg
        false

let testHead repo =
    printfn "\n=== Testing HEAD ==="
    match References.readHead repo with
    | Ok (Direct hash) ->
        printfn "✓ HEAD (direct): %s" hash
        true
    | Ok (Symbolic ref) ->
        printfn "✓ HEAD (symbolic): %s" ref
        true
    | Error msg ->
        printfn "✗ Failed to read HEAD: %s" msg
        false

let testCommitReading repo =
    printfn "\n=== Testing Commit Reading ==="
    match References.readHead repo with
    | Ok (Direct hash) ->
        match ReadObjects.readCommit repo hash with
        | Ok commit ->
            printfn "✓ Commit read successfully"
            printfn "  Tree: %s" commit.Tree
            printfn "  Parents: %A" commit.Parents
            printfn "  Author: %s <%s>" commit.Author.Name commit.Author.Email
            printfn "  Committer: %s <%s>" commit.Committer.Name commit.Committer.Email
            printfn "  Message: %s" commit.Message
            true
        | Error msg ->
            printfn "✗ Failed to read commit: %s" msg
            false
    | _ -> false

let testBranches repo =
    printfn "\n=== Testing Branches ==="
    match References.listBranches repo with
    | Ok branches ->
        printfn "✓ Found %d branches:" branches.Length
        for branch in branches do
            printfn "  - %s" branch
        branches.Length > 0
    | Error msg ->
        printfn "✗ Failed to list branches: %s" msg
        false

let testTags repo =
    printfn "\n=== Testing Tags ==="
    match References.listTags repo with
    | Ok tags ->
        printfn "✓ Found %d tags:" tags.Length
        for tag in tags do
            printfn "  - %s" tag
        tags.Length > 0
    | Error msg ->
        printfn "✗ Failed to list tags: %s" msg
        false

let testCommitHistory repo =
    printfn "\n=== Testing Commit History ==="
    match References.readHead repo with
    | Ok (Direct hash) ->
        let commits = CommitHistory.walkCommits repo hash |> Seq.truncate 5 |> Seq.toList
        if List.isEmpty commits then
            printfn "✗ No commits found"
            false
        else
            printfn "✓ Found %d commits (showing first 5):" commits.Length
            for commit, h in commits do
                let shortHash = h.[0..6]
                printfn "  %s %s" shortHash commit.Message
            true
    | Error msg ->
        printfn "✗ Failed to walk commits: %s" msg
        false
    | _ -> false

let testAncestry repo =
    printfn "\n=== Testing Ancestry ==="
    let branch1 = "feature"
    let branch2 = "master"
    
    match References.resolveReference repo ("refs/heads/" + branch1) with
    | Ok hash1 ->
        match References.resolveReference repo ("refs/heads/" + branch2) with
        | Ok hash2 ->
            match CommitHistory.isAncestor repo hash1 hash2 with
            | Ok true ->
                printfn "✓ %s is ancestor of %s" branch1 branch2
                true
            | Ok false ->
                printfn "✓ %s is NOT ancestor of %s" branch1 branch2
                true
            | Error msg ->
                printfn "✗ Failed to check ancestry: %s" msg
                false
        | Error msg ->
            printfn "✗ Failed to resolve %s: %s" branch2 msg
            false
    | Error msg ->
        printfn "✗ Failed to resolve %s: %s" branch1 msg
        false

let testTreeOperations repo =
    printfn "\n=== Testing Tree Operations ==="
    match References.readHead repo with
    | Ok (Direct hash) ->
        match ReadObjects.readCommit repo hash with
        | Ok commit ->
            match TreeOperations.listTree repo commit.Tree with
            | Ok entries ->
                printfn "✓ Listed %d root entries:" entries.Length
                for entry in entries do
                    printfn "  %s (%s)" entry.Path entry.Hash
                entries.Length > 0
            | Error msg ->
                printfn "✗ Failed to list tree: %s" msg
                false
        | Error msg ->
            printfn "✗ Failed to read commit: %s" msg
            false
    | Error msg ->
        printfn "✗ Failed to read HEAD: %s" msg
        false
    | _ -> false

let testFileReading repo =
    printfn "\n=== Testing File Reading ==="
    match RepositoryOperations.getFileContent repo "README.md" with
    | Ok content ->
        let text = System.Text.Encoding.UTF8.GetString(content)
        printfn "✓ README.md content:"
        printfn "  %s" (text.Trim())
        true
    | Error msg ->
        printfn "✗ Failed to read file: %s" msg
        false

let testStatus repo =
    printfn "\n=== Testing Status ==="
    match RepositoryOperations.getStatus repo with
    | Ok entries ->
        if Array.isEmpty entries then
            printfn "✓ Working tree is clean"
            true
        else
            printfn "✓ Found %d changed entries:" entries.Length
            for entry in entries do
                let indexStatus = 
                    match entry.IndexStatus with
                    | FileStatus.Unchanged -> "  "
                    | FileStatus.Added -> "+ "
                    | FileStatus.Modified -> "M "
                    | FileStatus.Deleted -> "- "
                    | FileStatus.Renamed -> "R "
                
                let workStatus =
                    match entry.WorkTreeStatus with
                    | FileStatus.Unchanged -> "  "
                    | FileStatus.Added -> "+ "
                    | FileStatus.Modified -> "M "
                    | FileStatus.Deleted -> "- "
                    | FileStatus.Renamed -> "R "
                
                printfn "  %s %s%s" indexStatus workStatus entry.Path
            entries.Length > 0 || Array.exists (fun e -> e.WorkTreeStatus <> FileStatus.Unchanged) entries
    | Error msg ->
        printfn "✗ Failed to get status: %s" msg
        false

let testConfig repo =
    printfn "\n=== Testing Config ==="
    match GitConfig.readConfig repo with
    | Ok config ->
        printfn "✓ Config read successfully"
        printfn "  Sections: %d" config.Sections.Length
        for section in config.Sections do
            printfn "  [%s%s]" section.Name (match section.Subsection with Some s -> $" \"{s}\"" | None -> "")
        config.Sections.Length > 0
    | Error msg ->
        printfn "✗ Failed to read config: %s" msg
        false

let main () =
    let repoPath = "/tmp/test-git-repo"
    
    printfn "========================================="
    printfn "F# Git Library - Comprehensive Tests"
    printfn "========================================="
    printfn "Testing against: %s" repoPath
    
    let results = ResizeArray()
    
    if not (testRepositoryOpening repoPath) then
        printfn "\n✗ CRITICAL: Cannot open repository, stopping tests"
        exit 1
    
    match Repository.openRepo repoPath with
    | Ok repo ->
        results.Add(testHead repo) |> ignore
        results.Add(testCommitReading repo) |> ignore
        results.Add(testBranches repo) |> ignore
        results.Add(testTags repo) |> ignore
        results.Add(testCommitHistory repo) |> ignore
        results.Add(testAncestry repo) |> ignore
        results.Add(testTreeOperations repo) |> ignore
        results.Add(testFileReading repo) |> ignore
        results.Add(testStatus repo) |> ignore
        results.Add(testConfig repo) |> ignore
        
        let passed = results |> Seq.filter id |> Seq.length
        let total = results.Count
        
        printfn "\n========================================="
        printfn "Test Results: %d / %d passed" passed total
        printfn "========================================="
        
        if passed = total then
            printfn "✓ ALL TESTS PASSED!"
            exit 0
        else
            let failed = total - passed
            printfn "✗ %d tests failed" failed
            exit 1
    | Error msg ->
        printfn "\n✗ CRITICAL: %s" msg
        exit 1
