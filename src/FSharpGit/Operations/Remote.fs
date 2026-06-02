namespace FSharpGit

open System
open System.IO

module Remote =
    
    let private parseRemoteConfig (content: string) : RemoteRepo[] =
        let lines = content.Split('\n')
        let mutable remotes = ResizeArray<RemoteRepo>()
        let mutable currentRemote: string option = None
        let mutable fetchUrl: string option = None
        let mutable pushUrl: string option = None
        
        let processLine (line: string) : unit =
            let trimmed = line.Trim()
            if trimmed.StartsWith "[remote \"" then
                match currentRemote, fetchUrl, pushUrl with
                | Some name, Some url, _ ->
                    let remote = { Name = name; FetchUrl = url; PushUrl = pushUrl } : RemoteRepo
                    remotes.Add remote |> ignore
                | _ -> ()
                
                let endQuote = trimmed.IndexOf('"', 9)
                if endQuote > 0 then
                    currentRemote <- Some trimmed.[9..endQuote - 1]
                    fetchUrl <- None
                    pushUrl <- None
                else
                    currentRemote <- None
                    fetchUrl <- None
                    pushUrl <- None
            elif trimmed.StartsWith "]" then
                match currentRemote, fetchUrl, pushUrl with
                | Some name, Some url, _ ->
                    let remote = { Name = name; FetchUrl = url; PushUrl = pushUrl } : RemoteRepo
                    remotes.Add remote |> ignore
                | _ -> ()
                currentRemote <- None
                fetchUrl <- None
                pushUrl <- None
            elif trimmed.StartsWith "url" then
                let parts = trimmed.Split('=')
                if parts.Length >= 2 then
                    fetchUrl <- Some (parts.[1].Trim())
                    if Option.isNone pushUrl then
                        pushUrl <- fetchUrl
            elif trimmed.StartsWith "pushurl" then
                let parts = trimmed.Split('=')
                if parts.Length >= 2 then
                    pushUrl <- Some (parts.[1].Trim())
        
        for line in lines do
            processLine line
        
        match currentRemote, fetchUrl, pushUrl with
        | Some name, Some url, _ ->
            let remote = { Name = name; FetchUrl = url; PushUrl = pushUrl } : RemoteRepo
            remotes.Add remote |> ignore
        | _ -> ()
        
        remotes.ToArray()
    
    let listRemotes (repo: Repo) : Result<RemoteRepo[], string> =
        try
            let configFile = Repository.getConfigFile repo
            if not (File.Exists configFile) then
                Ok [||]
            else
                let content = File.ReadAllText configFile
                Ok (parseRemoteConfig content)
        with
        | ex -> Error $"Failed to list remotes: {ex.Message}"
    
    let getRemote (repo: Repo) (name: string) : Result<RemoteRepo option, string> =
        match listRemotes repo with
        | Ok remotes ->
            Ok (remotes |> Array.tryFind (fun r -> r.Name = name))
        | Error msg -> Error msg
    
    let private gitProtocolFetch (remoteUrl: string) (refs: string[]) : Result<Map<string, GitHash>, string> =
        Error "Git protocol fetch not yet implemented"
    
    let private httpFetch (remoteUrl: string) (refs: string[]) : Result<Map<string, GitHash>, string> =
        Error "HTTP fetch not yet implemented"
    
    let private sshFetch (remoteUrl: string) (refs: string[]) : Result<Map<string, GitHash>, string> =
        Error "SSH fetch not yet implemented"
    
    let fetch (repo: Repo) (remote: string) : Result<unit, string> =
        match getRemote repo remote with
        | Ok (Some remoteObj) ->
            let protocol = 
                if remoteObj.FetchUrl.StartsWith "http://" || remoteObj.FetchUrl.StartsWith "https://" then
                    "http"
                elif remoteObj.FetchUrl.StartsWith "git://" then
                    "git"
                elif remoteObj.FetchUrl.Contains "@" && remoteObj.FetchUrl.Contains ":" then
                    "ssh"
                else
                    "file"
            
            match protocol with
            | "http" ->
                match httpFetch remoteObj.FetchUrl [||] with
                | Ok _ -> Ok ()
                | Error msg -> Error msg
            | "git" ->
                match gitProtocolFetch remoteObj.FetchUrl [||] with
                | Ok _ -> Ok ()
                | Error msg -> Error msg
            | "ssh" ->
                match sshFetch remoteObj.FetchUrl [||] with
                | Ok _ -> Ok ()
                | Error msg -> Error msg
            | "file" ->
                Ok ()
            | _ ->
                Error $"Unsupported protocol: {protocol}"
        | Ok None -> Error $"Remote not found: {remote}"
        | Error msg -> Error msg
    
    let push (repo: Repo) (remote: string) (branch: string) : Result<unit, string> =
        match getRemote repo remote with
        | Ok (Some remoteObj) ->
            Error "Push not yet implemented"
        | Ok None -> Error $"Remote not found: {remote}"
        | Error msg -> Error msg
    
    let lsRemote (repo: Repo) (remote: string) : Result<Map<string, GitHash>, string> =
        match getRemote repo remote with
        | Ok (Some remoteObj) ->
            Error "ls-remote not yet implemented"
        | Ok None -> Error $"Remote not found: {remote}"
        | Error msg -> Error msg
    
    let addRemote (repo: Repo) (name: string) (url: string) : Result<unit, string> =
        try
            let configFile = Repository.getConfigFile repo
            let mutable content = if File.Exists configFile then File.ReadAllText configFile else ""
            
            let section = $"\n[remote \"{name}\"]\n\turl = {url}\n"
            if content.Contains($"[remote \"{name}\"]") then
                Error $"Remote {name} already exists"
            else
                content <- content + section
                File.WriteAllText(configFile, content)
                Ok ()
        with
        | ex -> Error $"Failed to add remote: {ex.Message}"
    
    let removeRemote (repo: Repo) (name: string) : Result<unit, string> =
        try
            let configFile = Repository.getConfigFile repo
            if not (File.Exists configFile) then
                Ok ()
            else
                let content = File.ReadAllText configFile
                let startTag = $"[remote \"{name}\"]"
                let startIndex = content.IndexOf startTag
                if startIndex < 0 then
                    Error $"Remote {name} not found"
                else
                    let endIndex = content.IndexOf("\n[", startIndex + 1)
                    let newContent =
                        if endIndex > 0 then
                            content.[0..startIndex - 1] + content.[endIndex..]
                        else
                            content.[0..startIndex - 1]
                    File.WriteAllText(configFile, newContent)
                    Ok ()
        with
        | ex -> Error $"Failed to remove remote: {ex.Message}"
