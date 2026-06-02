#r "../src/FSharpGit/bin/Release/net10.0/FSharpGit.dll"

open FSharpGit
open System.IO

let content = File.ReadAllText "/tmp/test-git-repo/.git/HEAD"
printfn "HEAD content: [%s]" content

let repo = Repository.openRepo "/tmp/test-git-repo" |> Result.defaultWith (fun _ -> {Path=""; GitDir=""; WorkTree=None})
printfn "Repo opened"

match References.readHead repo with
| Ok (Direct hash) -> printfn "HEAD direct: %s" hash
| Ok (Symbolic ref) -> printfn "HEAD symbolic: %s" ref
| Error msg -> printfn "Error: %s" msg
