namespace Rbt.Git

open System
open System.IO
open System.Text
open System.Text.RegularExpressions

type ConfigSection = {
    Name: string
    Subsection: string option
    Entries: (string * string)[]
} with
    member section.Values =
        section.Entries |> Array.fold (fun m (k, v) -> Map.add (k.ToLowerInvariant()) v m) Map.empty

type Config = { Sections: ConfigSection[] }

module GitConfig =
    let private same a b = String.Equals(a, b, StringComparison.OrdinalIgnoreCase)
    let private decode (text: string) =
        let b = StringBuilder()
        let mutable quoted = false
        let mutable escaped = false
        let mutable pending = StringBuilder()
        let mutable stop = false
        for c in text do
            if not stop then
                if escaped then
                    let decoded = match c with | 'n' -> '\n' | 't' -> '\t' | 'b' -> '\b' | '"' -> '"' | '\\' -> '\\' | _ -> raise (InvalidDataException "Invalid config escape")
                    b.Append(pending.ToString()).Append(decoded) |> ignore
                    pending.Clear() |> ignore
                    escaped <- false
                elif c = '\\' then escaped <- true
                elif c = '"' then quoted <- not quoted
                elif not quoted && (c = '#' || c = ';') then stop <- true
                elif not quoted && Char.IsWhiteSpace c then pending.Append(c) |> ignore
                else
                    b.Append(pending.ToString()).Append(c) |> ignore
                    pending.Clear() |> ignore
        if quoted || escaped then raise (InvalidDataException "Unterminated config value")
        b.ToString()

    let private quote (s: string) =
        "\"" + s.Replace("\\", "\\\\").Replace("\"", "\\\"").Replace("\n", "\\n").Replace("\t", "\\t").Replace("\b", "\\b") + "\""

    let private makeSection name subsection (entries: seq<string * string>) =
        { Name = name; Subsection = subsection; Entries = Seq.toArray entries }

    let parseConfig (content: string) : Config =
        let sections = ResizeArray<ConfigSection>()
        let mutable name = None
        let mutable subsection = None
        let entries = ResizeArray<string * string>()
        let flush () =
            match name with
            | Some n -> sections.Add(makeSection n subsection entries); entries.Clear()
            | None -> ()
        let sectionRegex = Regex("^\\[([A-Za-z0-9.-]+)(?:\\s+\"((?:[^\"\\\\]|\\\\.)*)\")?\\]\\s*(?:[#;].*)?$")
        let logicalLines = ResizeArray<string>()
        let mutable line = ""
        for raw in content.Replace("\r\n", "\n").Split('\n') do
            line <- line + raw
            let mutable count = 0
            let mutable i = line.Length - 1
            while i >= 0 && line.[i] = '\\' do
                count <- count + 1
                i <- i - 1
            if count % 2 = 1 then
                line <- line.Substring(0, line.Length - 1)
            else
                logicalLines.Add line
                line <- ""
        if line <> "" then raise (InvalidDataException "Unterminated config continuation")
        for raw in logicalLines do
            let text = raw.Trim()
            if text = "" || text.StartsWith("#") || text.StartsWith(";") then ()
            elif text.StartsWith("[") then
                let m = sectionRegex.Match text
                if not m.Success then raise (InvalidDataException $"Invalid config section: {text}")
                flush ()
                let sectionName = m.Groups.[1].Value
                if m.Groups.[2].Success then
                    name <- Some(sectionName.ToLowerInvariant())
                    subsection <- Some(decode ("\"" + m.Groups.[2].Value + "\""))
                elif sectionName.Contains('.') then
                    let index = sectionName.IndexOf('.')
                    name <- Some(sectionName.Substring(0, index).ToLowerInvariant())
                    subsection <- Some(sectionName.Substring(index + 1).ToLowerInvariant())
                else
                    name <- Some(sectionName.ToLowerInvariant())
                    subsection <- None
            else
                if name.IsNone then raise (InvalidDataException "Config variable outside section")
                let m = Regex.Match(text, "^([A-Za-z][A-Za-z0-9-]*)(?:\\s*=\\s*(.*)|\\s*(?:[#;].*)?)$")
                if not m.Success then raise (InvalidDataException $"Invalid config variable: {text}")
                let key = m.Groups.[1].Value.ToLowerInvariant()
                let value = if m.Groups.[2].Success then decode m.Groups.[2].Value else "true"
                entries.Add(key, value)
        flush ()
        { Sections = sections.ToArray() }

    let serializeConfig (config: Config) : string =
        let b = StringBuilder()
        for section in config.Sections do
            match section.Subsection with
            | Some sub -> b.Append('[').Append(section.Name).Append(' ').Append(quote sub).AppendLine("]") |> ignore
            | None -> b.Append('[').Append(section.Name).AppendLine("]") |> ignore
            for key, value in section.Entries do b.Append('\t').Append(key).Append(" = ").AppendLine(quote value) |> ignore
        b.ToString()

    let readConfig (repo: Repo) : Result<Config, string> =
        try
            let file = Repository.getConfigFile repo
            Ok (if File.Exists file then parseConfig (File.ReadAllText file) else { Sections = [||] })
        with ex -> Error $"Failed to read config: {ex.Message}"

    let private lockedWrite repo (f: unit -> Result<Config, string>) =
        try
            let file = Repository.getConfigFile repo
            let lockPath = file + ".lock"
            use stream = new FileStream(lockPath, FileMode.CreateNew, FileAccess.Write, FileShare.None)
            try
                match f () with
                | Error error -> Error error
                | Ok config ->
                    let bytes = Encoding.UTF8.GetBytes(serializeConfig config)
                    stream.Write(bytes, 0, bytes.Length)
                    stream.Flush(true)
                    stream.Dispose()
                    File.Move(lockPath, file, true)
                    Ok ()
            finally
                stream.Dispose()
                if File.Exists lockPath then File.Delete lockPath
        with ex -> Error $"Failed to write config: {ex.Message}"

    let writeConfig repo config = lockedWrite repo (fun () -> Ok config)
    let private matches section subsection (s: ConfigSection) = same s.Name section && s.Subsection = subsection
    let getValue (config: Config) section subsection key =
        config.Sections |> Array.filter (matches section subsection) |> Array.collect (fun s -> s.Entries)
        |> Array.filter (fun (k, _) -> same k key) |> Array.tryLast |> Option.map snd
    let getValues (config: Config) section subsection key =
        config.Sections |> Array.filter (matches section subsection) |> Array.collect (fun s -> s.Entries)
        |> Array.choose (fun (k, v) -> if same k key then Some v else None)
    let removeValue (config: Config) section subsection key =
        { Sections = config.Sections |> Array.map (fun s -> if matches section subsection s then makeSection s.Name s.Subsection (s.Entries |> Array.filter (fun (k, _) -> not (same k key))) else s) }
    let setValue (config: Config) section subsection key value =
        let stripped = removeValue config section subsection key
        let entry = makeSection (section.ToLowerInvariant()) subsection [| key.ToLowerInvariant(), value |]
        { Sections = Array.append stripped.Sections [| entry |] }
    let removeSection (config: Config) section subsection =
        { Sections = config.Sections |> Array.filter (matches section subsection >> not) }
    let getAllValues (config: Config) section subsection =
        config.Sections |> Array.filter (matches section subsection) |> Array.collect (fun s -> s.Entries)
        |> Array.fold (fun m (k, v) -> Map.add (k.ToLowerInvariant()) v m) Map.empty
    let getSections (config: Config) sectionName = config.Sections |> Array.filter (fun s -> same s.Name sectionName)
    let hasSection (config: Config) section subsection = config.Sections |> Array.exists (matches section subsection)
    let updateConfig (repo: Repo) (transform: Config -> Result<Config, string>) : Result<unit, string> =
        lockedWrite repo (fun () -> readConfig repo |> Result.bind transform)
    let private change repo f = updateConfig repo (f >> Ok)
    let updateValue repo section subsection key value = change repo (fun c -> setValue c section subsection key value)
    let removeValueFromRepo repo section subsection key = change repo (fun c -> removeValue c section subsection key)
    let removeSectionFromRepo repo section subsection = change repo (fun c -> removeSection c section subsection)
    let getValueFromRepo repo section subsection key = readConfig repo |> Result.map (fun c -> getValue c section subsection key)
