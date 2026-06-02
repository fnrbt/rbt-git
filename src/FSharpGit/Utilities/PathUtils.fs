namespace FSharpGit

open System
open System.IO

module PathUtils =
    
    let normalizeGitPath (path: string) : string =
        path.Replace('\\', '/').TrimStart('/').TrimEnd('/')
    
    let isSubPath (parent: string) (child: string) : bool =
        let normalizedParent = normalizeGitPath parent
        let normalizedChild = normalizeGitPath child
        normalizedChild.StartsWith(normalizedParent + "/") || normalizedChild = normalizedParent
    
    let joinPaths (parts: string[]) : string =
        parts
        |> Array.map normalizeGitPath
        |> Array.filter (String.IsNullOrWhiteSpace >> not)
        |> String.concat "/"
    
    let getPathParent (path: string) : string option =
        let normalized = normalizeGitPath path
        let lastSlash = normalized.LastIndexOf '/'
        if lastSlash < 0 then
            None
        else
            Some normalized.[0..lastSlash - 1]
    
    let getPathName (path: string) : string =
        let normalized = normalizeGitPath path
        let lastSlash = normalized.LastIndexOf '/'
        if lastSlash < 0 then
            normalized
        else
            normalized.[lastSlash + 1..]
    
    let splitPath (path: string) : string[] =
        let normalized = normalizeGitPath path
        if String.IsNullOrWhiteSpace normalized then
            [||]
        else
            normalized.Split('/')
    
    let relativePath (basePath: string) (targetPath: string) : string option =
        let normalizedBase = normalizeGitPath basePath
        let normalizedTarget = normalizeGitPath targetPath
        
        if isSubPath normalizedBase normalizedTarget then
            let relative = normalizedTarget.[normalizedBase.Length + 1..]
            Some relative
        else
            None
    
    let isAbsolutePath (path: string) : bool =
        (path.Length >= 2 && Char.IsLetter path.[0] && path.[1] = ':') ||
        path.StartsWith("/") ||
        path.StartsWith("\\\\")
    
    let toPlatformPath (path: string) : string =
        if Path.DirectorySeparatorChar = '\\' then
            path.Replace('/', '\\')
        else
            path.Replace('\\', '/')
    
    let toGitPath (path: string) : string =
        path.Replace('\\', '/')
    
    let combinePaths (basePath: string) (relativePath: string) : string =
        joinPaths [| basePath; relativePath |]
    
    let isSamePath (path1: string) (path2: string) : bool =
        normalizeGitPath path1 = normalizeGitPath path2
    
    let getPathDepth (path: string) : int =
        let normalized = normalizeGitPath path
        if String.IsNullOrWhiteSpace normalized then
            0
        else
            normalized.Split('/') |> Array.length
    
    let commonPathPrefix (path1: string) (path2: string) : string =
        let parts1 = splitPath path1
        let parts2 = splitPath path2
        
        let mutable commonParts = ResizeArray()
        let mutable i = 0
        while i < Array.length parts1 && i < Array.length parts2 && parts1.[i] = parts2.[i] do
            commonParts.Add parts1.[i]
            i <- i + 1
        
        if commonParts.Count = 0 then
            ""
        else
            String.concat "/" (commonParts.ToArray())
    
    let normalizePath (path: string) : string =
        let normalized = path.Replace('\\', '/')
        let parts = normalized.Split('/', StringSplitOptions.RemoveEmptyEntries)
        let result = ResizeArray()
        
        for part in parts do
            if part = ".." && result.Count > 0 then
                result.RemoveAt(result.Count - 1)
            elif part <> "." then
                result.Add part
        
        String.concat "/" (result.ToArray())
