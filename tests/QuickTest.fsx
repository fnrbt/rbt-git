#r "/home/dave/Dev/fsgit/src/FSharpGit/bin/Release/net10.0/FSharpGit.dll"

printfn "=== Testing FSharpGit ==="

printfn "\n1. Repository.locateRepo:"
let r = Repository.locateRepo "/tmp/test-git-repo"
printfn "Done: %s" r.Path

printfn "\n2. Repository.getHeadFile:"
let headPath = Repository.getHeadFile r
printfn "Path: %s" headPath

printfn "\n3. File.Exists:"
let exists = System.IO.File.Exists headPath
printfn "Exists: %b" exists

if exists then
    let content = System.IO.File.ReadAllText headPath
    printfn "Content: %s" content
    printfn "\nSuccess!"
else
    printfn "\nFailed!"
