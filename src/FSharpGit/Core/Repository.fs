namespace FSharpGit

open System
open System.IO

module Repository =
    
    let locateRepo (path: string) : Repo =
        let rec findGitDir current =
            let gitPath = Path.Combine(current, ".git")
            if Directory.Exists gitPath then
                let gitFile = Path.Combine(current, ".git")
                if File.Exists gitFile then
                    let content = File.ReadAllText gitFile
                    if content.StartsWith "gitdir:" then
                        let gitDir = content.[8..].Trim()
                        Path.Combine(current, gitDir)
                    else
                        gitPath
                else
                    gitPath
            else
                let parent = Directory.GetParent current
                if isNull parent then
                    raise (DirectoryNotFoundException $"Not a git repository: {path}")
                else
                    findGitDir parent.FullName
        
        let fullPath = Path.GetFullPath path
        let isDir = Directory.Exists fullPath
        let root = if isDir then fullPath else Path.GetDirectoryName fullPath
        
        let gitDir = findGitDir root
        let workTree = 
            if Directory.Exists root then
                Some root
            else
                None
        
        { Path = root; GitDir = gitDir; WorkTree = workTree }
    
    let openRepo (path: string) : Result<Repo, string> =
        try
            Ok (locateRepo path)
        with
        | :? DirectoryNotFoundException as ex -> Error ex.Message
        | ex -> Error $"Failed to open repository: {ex.Message}"
    
    let gitPath (repo: Repo) (segments: string[]) : string =
        Path.Combine([| repo.GitDir |] |> Array.append segments)
    
    let hasWorkTree (repo: Repo) : bool =
        Option.isSome repo.WorkTree
    
    let workTreePath (repo: Repo) (segments: string[]) : string =
        match repo.WorkTree with
        | Some wt -> Path.Combine([| wt |] |> Array.append segments)
        | None -> 
            raise (NotSupportedException "Bare repository does not have a working tree")
    
    let isBare (repo: Repo) : bool =
        not (hasWorkTree repo)
    
    let getHeadFile (repo: Repo) : string =
        gitPath repo [| "HEAD" |]
    
    let getConfigFile (repo: Repo) : string =
        gitPath repo [| "config" |]
    
    let getIndexPath (repo: Repo) : string =
        gitPath repo [| "index" |]
    
    let getObjectsDir (repo: Repo) : string =
        gitPath repo [| "objects" |]
    
    let getRefsDir (repo: Repo) : string =
        gitPath repo [| "refs" |]
    
    let getHeadsDir (repo: Repo) : string =
        Path.Combine(getRefsDir repo, "heads")
    
    let getTagsDir (repo: Repo) : string =
        Path.Combine(getRefsDir repo, "tags")
    
    let getRemotesDir (repo: Repo) : string =
        Path.Combine(getRefsDir repo, "remotes")
    
    let getPackedRefsFile (repo: Repo) : string =
        gitPath repo [| "packed-refs" |]
    
    let getPackDir (repo: Repo) : string =
        Path.Combine(getObjectsDir repo, "pack")
    
    let getLooseObjectPath (repo: Repo) (hash: GitHash) : string =
        let dir = hash.[..1]
        let file = hash.[2..]
        Path.Combine(getObjectsDir repo, dir, file)
    
    let getPackIndexPath (repo: Repo) (packName: string) : string =
        Path.Combine(getPackDir repo, $"{packName}.idx")
    
    let getPackFilePath (repo: Repo) (packName: string) : string =
        Path.Combine(getPackDir repo, $"{packName}.pack")
    
    let packFilesExist (repo: Repo) : (string * string) [] =
        if not (Directory.Exists (getPackDir repo)) then
            [||]
        else
            Directory.GetFiles(getPackDir repo, "*.idx")
            |> Array.map (fun idxPath ->
                let name = Path.GetFileNameWithoutExtension idxPath
                let packPath = Path.ChangeExtension(idxPath, ".pack")
                (name, packPath))
    
    let branchFilePath (repo: Repo) (branch: string) : string =
        Path.Combine(getHeadsDir repo, branch)
    
    let tagFilePath (repo: Repo) (tag: string) : string =
        Path.Combine(getTagsDir repo, tag)
    
    let remoteBranchFilePath (repo: Repo) (remote: string) (branch: string) : string =
        Path.Combine(getRemotesDir repo, remote, branch)
