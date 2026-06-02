let loadAndTest () =
    try
        #r "../src/FSharpGit/bin/Release/net10.0/FSharpGit.dll"
        open FSharpGit
        
        printfn "FSharpGit loaded successfully!"
        printfn "Testing Repository.locateRepo..."
        
        let repo = Repository.locateRepo "/tmp/test-git-repo"
        printfn "Success: %s" repo.Path
        printfn "GitDir: %s" repo.GitDir
        
        printfn "All modules loaded and working!"
    with
    | :? ex ->
        printfn "Error loading FSharpGit: %s" ex.Message
        exit 1
    | ex ->
        printfn "Error: %s" ex.Message
        exit 1

loadAndTest ()
