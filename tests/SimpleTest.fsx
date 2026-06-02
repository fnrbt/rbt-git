open System
open System.IO

#r "../src/FSharpGit/bin/Release/net10.0/FSharpGit.dll"

open FSharpGit

let repoPath = "/tmp/test-git-repo"

use writer = File.CreateText "/tmp/test-output.txt"
writer.WriteLine "Starting tests..."

match Repository.openRepo repoPath with
| Ok repo ->
    writer.WriteLine ("Repository opened: " + repo.Path) |> ignore
    
    match References.readHead repo with
    | Ok (Direct hash) -> writer.WriteLine ("HEAD: " + hash) |> ignore
    | Ok (Symbolic ref) -> writer.WriteLine ("HEAD symbolic: " + ref) |> ignore
    | Error msg -> writer.WriteLine ("Error: " + msg) |> ignore
    
    match References.listBranches repo with
    | Ok branches ->
        writer.WriteLine ("Branches: " + (String.Join(", ", branches))) |> ignore
    | Error msg -> writer.WriteLine ("Error listing branches: " + msg) |> ignore
    
    match References.listTags repo with
    | Ok tags ->
        writer.WriteLine ("Tags: " + (String.Join(", ", tags))) |> ignore
    | Error msg -> writer.WriteLine ("Error listing tags: " + msg) |> ignore
    
    writer.WriteLine "All tests completed!" |> ignore
| Error msg ->
    writer.WriteLine ("Error: " + msg) |> ignore

writer.Flush() |> ignore
writer.Close()

printfn "Output written to /tmp/test-output.txt"
