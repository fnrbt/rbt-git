#r "../src/FSharpGit/bin/Release/net10.0/FSharpGit.dll"

open FSharpGit
open System.IO

printfn "Test 1: Direct open with Repository.locateRepo"
let repo1 = Repository.locateRepo "/tmp/test-git-repo"
printfn "  Path: %s" repo1.Path
printfn "  GitDir: %s" repo1.GitDir
printfn "  WorkTree: %s" (match repo1.WorkTree with Some wt -> wt | None -> "None")

printfn "\nTest 2: Check HEAD file exists"
let headPath = Path.Combine(repo1.GitDir, "HEAD")
let headExists = File.Exists headPath
printfn "  HEAD exists: %b" headExists
if headExists then
    let content = File.ReadAllText headPath
    printfn "  HEAD content: %s" content

printfn "\nTest 3: Try to read HEAD using References.readHead"
match References.readHead repo1 with
| Ok (Direct hash) -> printfn "  HEAD direct: %s" hash
| Ok (Symbolic ref) -> printfn "  HEAD symbolic: %s" ref
| Error msg -> printfn "  Error: %s" msg
