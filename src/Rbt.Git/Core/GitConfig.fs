namespace Rbt.Git

open System
open System.IO
open System.Text.RegularExpressions

type ConfigSection = {
    Name: string
    Subsection: string option
    Values: Map<string, string>
}

type Config = {
    Sections: ConfigSection[]
}

module GitConfig =
    
    let private parseSectionName (line: string) : (string * string option) option =
        let regex = Regex(@"^\s*\[(.+?)(?:\s+""(.+?)"")?\]\s*$")
        let m = regex.Match line
        if m.Success then
            Some (m.Groups.[1].Value, if m.Groups.[2].Success then Some m.Groups.[2].Value else None)
        else
            None
    
    let private parseKeyValue (line: string) : (string * string) option =
        let trimmed = line.Trim()
        if trimmed.StartsWith "#" then
            None
        elif trimmed.Contains "=" then
            let parts = trimmed.Split('=', 2)
            let key = parts.[0].Trim()
            let value = if parts.Length > 1 then parts.[1].Trim() else ""
            Some (key, value)
        else
            None
    
    let parseConfig (content: string) : Config =
        let lines = content.Split('\n')
        let mutable sections = ResizeArray<ConfigSection>()
        let mutable currentSection: ConfigSection option = None
        let mutable values = Map.empty
        
        for line in lines do
            match parseSectionName line with
            | Some (name, subsection) ->
                match currentSection with
                | Some sec -> sections.Add { sec with Values = values }
                | None -> ()
                
                currentSection <- Some { Name = name; Subsection = subsection; Values = Map.empty }
                values <- Map.empty
            | None ->
                match parseKeyValue line with
                | Some (key, value) ->
                    values <- Map.add key value values
                | None -> ()
        
        match currentSection with
        | Some sec -> sections.Add { sec with Values = values }
        | None -> ()
        
        { Sections = sections.ToArray() }
    
    let serializeConfig (config: Config) : string =
        let sb = Text.StringBuilder()
        
        for section in config.Sections do
            match section.Subsection with
            | Some sub ->
                sb.AppendLine $"[{section.Name} \"{sub}\"]" |> ignore
            | None ->
                sb.AppendLine $"[{section.Name}]" |> ignore
            
            for kvp in section.Values do
                sb.AppendLine $"\t{kvp.Key} = {kvp.Value}" |> ignore
            
            sb.AppendLine() |> ignore
        
        sb.ToString().TrimEnd()
    
    let readConfig (repo: Repo) : Result<Config, string> =
        try
            let configFile = Repository.getConfigFile repo
            if not (File.Exists configFile) then
                Ok { Sections = [||] }
            else
                let content = File.ReadAllText configFile
                Ok (parseConfig content)
        with
        | ex -> Error $"Failed to read config: {ex.Message}"
    
    let writeConfig (repo: Repo) (config: Config) : Result<unit, string> =
        try
            let configFile = Repository.getConfigFile repo
            let content = serializeConfig config
            File.WriteAllText(configFile, content)
            Ok ()
        with
        | ex -> Error $"Failed to write config: {ex.Message}"
    
    let getValue (config: Config) (section: string) (subsection: string option) (key: string) : string option =
        config.Sections
        |> Array.tryFind (fun s -> s.Name = section && s.Subsection = subsection)
        |> Option.bind (fun s -> Map.tryFind key s.Values)
    
    let setValue (config: Config) (section: string) (subsection: string option) (key: string) (value: string) : Config =
        let newSections = 
            config.Sections
            |> Array.map (fun s ->
                if s.Name = section && s.Subsection = subsection then
                    { s with Values = Map.add key value s.Values }
                else
                    s)
        
        let hasSection = 
            config.Sections
            |> Array.exists (fun s -> s.Name = section && s.Subsection = subsection)
        
        if hasSection then
            { Sections = newSections }
        else
            let newSection = { Name = section; Subsection = subsection; Values = Map.ofArray [| (key, value) |] }
            { Sections = Array.append config.Sections [| newSection |] }
    
    let removeValue (config: Config) (section: string) (subsection: string option) (key: string) : Config =
        { Sections = config.Sections |> Array.map (fun s ->
            if s.Name = section && s.Subsection = subsection then
                { s with Values = Map.remove key s.Values }
            else
            s) }
    
    let removeSection (config: Config) (section: string) (subsection: string option) : Config =
        { Sections = config.Sections |> Array.filter (fun s -> not (s.Name = section && s.Subsection = subsection)) }
    
    let getAllValues (config: Config) (section: string) (subsection: string option) : Map<string, string> =
        config.Sections
        |> Array.tryFind (fun s -> s.Name = section && s.Subsection = subsection)
        |> Option.map (fun s -> s.Values)
        |> Option.defaultValue Map.empty
    
    let getSections (config: Config) (sectionName: string) : ConfigSection[] =
        config.Sections
        |> Array.filter (fun s -> s.Name = sectionName)
    
    let hasSection (config: Config) (section: string) (subsection: string option) : bool =
        config.Sections
        |> Array.exists (fun s -> s.Name = section && s.Subsection = subsection)
    
    let updateValue (repo: Repo) (section: string) (subsection: string option) (key: string) (value: string) : Result<unit, string> =
        match readConfig repo with
        | Ok config ->
            let newConfig = setValue config section subsection key value
            writeConfig repo newConfig
        | Error msg -> Error msg
    
    let removeValueFromRepo (repo: Repo) (section: string) (subsection: string option) (key: string) : Result<unit, string> =
        match readConfig repo with
        | Ok config ->
            let newConfig = removeValue config section subsection key
            writeConfig repo newConfig
        | Error msg -> Error msg
    
    let removeSectionFromRepo (repo: Repo) (section: string) (subsection: string option) : Result<unit, string> =
        match readConfig repo with
        | Ok config ->
            let newConfig = removeSection config section subsection
            writeConfig repo newConfig
        | Error msg -> Error msg
    
    let getValueFromRepo (repo: Repo) (section: string) (subsection: string option) (key: string) : Result<string option, string> =
        match readConfig repo with
        | Ok config -> Ok (getValue config section subsection key)
        | Error msg -> Error msg
