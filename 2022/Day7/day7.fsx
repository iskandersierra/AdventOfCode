open System
open System.IO

#nowarn "0025"

type FileSystem = FolderTree * CurrentDirectory
and FolderTree = Map<string, FileOrFolder>
and FileOrFolder =
    | Folder of name: string * children: FolderTree
    | File of name: string * size: int
and CurrentDirectory = string list

let matchRegex regex line =
    let m = System.Text.RegularExpressions.Regex.Match(line, regex)
    if m.Success then Some m else None

let (|ChangeDirectoryRoot|_|) =
    matchRegex @"^\$\s+cd\s+\/\s*$"
    >> Option.map (fun _ -> ())
let (|ChangeDirectoryParent|_|) =
    matchRegex @"^\$\s+cd\s+\.\.\s*$"
    >> Option.map (fun _ -> ())
let (|ChangeDirectoryRelative|_|) =
    matchRegex @"^\$\s+cd\s+(?<folder>[^\s]+)\s*$"
    >> Option.map (fun m -> m.Groups.["folder"].Value)
let (|ListDirectory|_|) =
    matchRegex @"^\$\s+ls\s*$"
    >> Option.map (fun _ -> ())
let (|FolderInfo|_|) =
    matchRegex @"^dir\s+(?<name>[^\s]+)\s*$"
    >> Option.map (fun m -> m.Groups.["name"].Value)
let (|FileInfo|_|) =
    matchRegex @"^(?<size>\d+)\s+(?<name>[^\s]+)\s*$"
    >> Option.map (fun m -> m.Groups.["name"].Value, int m.Groups.["size"].Value)

let createFolder name (folders, current) =
    let rec loop folders current =
        match current with
        | [] -> folders |> Map.add name (Folder(name, Map.empty))
        | folder :: current ->
            match folders |> Map.tryFind folder with
            | None -> failwithf "Folder %s does not exist" folder
            | Some(File(name, _)) -> failwithf "Cannot cd into file %s" name
            | Some(Folder(name, children)) ->
                let children = loop children current
                folders |> Map.add name (Folder(name, children))
    loop folders (List.rev current)

let createFile name size (folders, current) =
    let rec loop folders current =
        match current with
        | [] -> folders |> Map.add name (File(name, size))
        | folder :: current ->
            match folders |> Map.tryFind folder with
            | None -> failwithf "Folder %s does not exist" folder
            | Some(File(name, _)) -> failwithf "Cannot cd into file %s" name
            | Some(Folder(name, children)) ->
                let children = loop children current
                folders |> Map.add name (Folder(name, children))
    loop folders (List.rev current)

let parseFileSystem lines : FileSystem =
    let rec loop fileSystem lines =
        match lines, fileSystem with
        | [], fileSystem -> fileSystem
        | ChangeDirectoryRoot :: lines, (folders, _) ->
            loop (folders, []) lines
        | ChangeDirectoryParent :: lines, (folders, _ :: current) ->
            loop (folders, current) lines
        | ChangeDirectoryParent :: _, _ ->
            failwithf "Cannot change directory parent from root"
        | ChangeDirectoryRelative name :: lines, (folders, current) ->
            loop (createFolder name (folders, current), name :: current) lines
        | ListDirectory :: lines, (folders, current) ->
            loop fileSystem lines
        | FolderInfo name :: lines, (folders, current) ->
            loop (createFolder name (folders, current), current) lines
        | FileInfo (name, size) :: lines, (folders, current) ->
            loop (createFile name size (folders, current), current) lines
        | line :: lines, _ ->
            failwithf "Unknown instruction %s" line
    loop (Map.empty, []) (lines |> Seq.toList)

let listFoldersWithSize folders : (string * int) list =
    let result = ResizeArray()
    let rec loopMany (folders: FolderTree) =
        folders.Values
        |> Seq.fold (fun size folder -> size + loopOne folder) 0
    and loopOne folder =
        match folder with
        | File(_, size) -> size
        | Folder(name, folders) ->
            let size = loopMany folders
            result.Add(name, size)
            size
    let totalSize = loopMany folders
    result.Add("/", totalSize)
    result |> Seq.toList

let part1 lines =
    lines
        |> parseFileSystem
        |> fst
        |> listFoldersWithSize
        |> Seq.filter (fun (_, size) -> size <= 100_000)
        |> Seq.map snd
        |> Seq.sum

let part2 lines =
    let sizes =
        lines
        |> parseFileSystem
        |> fst
        |> listFoldersWithSize
        |> List.map snd
    let totalSpace = List.max sizes
    let neededSpace = totalSpace - 40_000_000
    sizes
        |> List.filter (fun size -> size > neededSpace)
        |> List.sort
        |> List.head

let [| fileName; part |] = Environment.GetCommandLineArgs().[2..3]

File.ReadAllLines fileName
    |> if part = "1" then part1 else part2
    |> printfn "%A"
