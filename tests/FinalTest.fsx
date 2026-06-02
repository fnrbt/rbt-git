#r "src/FSharpGit/bin/Release/net10.0/FSharpGit.dll"

open System.IO

printfn "Checking if FSharpGit namespace exists..."

try
    let r = Repository.locateRepo "/tmp/test-git-repo"
    printfn "Success! Repository located at: %s" r.Path
    printfn "GitDir: %s" r.GitDir
with ex ->
    printfn "Error: %s" ex.Message
    exit 1
