namespace AdventOfCode.Solutions.Day7

open AdventOfCode.Common

module Solution =
    type ChangeDirectoryCommand = { RelativeFolderName: string; }
    type ListDirectoryCommand = { Output: string array }
    type Command =
        | ChangeDirectory of ChangeDirectoryCommand
        | ListDirectory of ListDirectoryCommand

    type FileTreeItem =
        | Directory of DirectoryInfo
        | File of FileInfo
    and DirectoryInfo = { Name: string; Items: FileTreeItem list}
    and FileInfo = { Name: string; Size: int }

    let isDirectoryWithName name fileTreeItem =
        match fileTreeItem with
        | Directory dirInfo when dirInfo.Name = name -> true
        | _ -> false

    let getDirectoryWithName dirName directory =
        let idx = directory.Items |> List.findIndex (isDirectoryWithName dirName)
        let directory =
            match directory.Items[idx] with
            | Directory dir -> dir
            | File _ -> failwith "Expected that the found item was directory, but it wasnt"
        (idx, directory)

    let rec insertItems (directory: DirectoryInfo) (cwd: string list) (items: FileTreeItem list) =
        match cwd with
        | [] -> { directory with Items = items}
        | head :: tail when head = "/" -> insertItems directory tail items
        | head :: tail -> 
            let (newFolderIdx, folderToInsertInto) = getDirectoryWithName head directory
            let newFolder = insertItems folderToInsertInto tail items 
            {directory with Items = List.updateAt newFolderIdx (Directory newFolder) directory.Items }

    let createFile fileDesc =
        let fileSplit = Helpers.splitString " " fileDesc
        match Array.length fileSplit with
        | 2 -> { Name = fileSplit[1];  Size = int fileSplit[0] }
        | _ -> failwith (sprintf "File descriptor %s has invalid format" fileDesc)

    let createFileTreeItem itemDesc =
        let charListToString charLst =
            new string(charLst |> List.toArray)

        match itemDesc |> Seq.toList with
        | 'd' :: 'i' :: 'r' :: ' ' :: dirName -> Directory { Name = (charListToString dirName) + "/"; Items = [] }
        | fileDesc -> File (createFile (charListToString fileDesc))
    let applyLSCommand (command: ListDirectoryCommand) (cwd: string list) (root: DirectoryInfo) =
        command.Output
        |> Array.filter (fun str -> not (str = ""))
        |> Array.map createFileTreeItem
        |> Array.toList
        |> insertItems root cwd

    let parseCommand str =
        let lines = Helpers.splitLines str
        let commandInput = lines[0] |> Helpers.splitString " " |> Array.removeAt 0
        match commandInput[0] with
        | "cd" -> ChangeDirectory { RelativeFolderName = commandInput[1] }
        | "ls" -> ListDirectory { Output = Array.removeAt 0 lines }
        | _ -> failwith (sprintf "Unknown command %s" commandInput[0])

    let parseCommands input =   
        input
        |> Helpers.splitString "\$"
        |> Array.removeManyAt 0 2
        |> Array.map parseCommand
        |> Array.toList
    
    let buildRoot commands =
        let buildFileTree (cwd: string list, root: DirectoryInfo) (command: Command) =
            let (newCwd, newRoot)=
                match command with
                | ChangeDirectory cdCommand -> 
                    match cdCommand.RelativeFolderName with
                    | ".." -> (List.removeAt (List.length cwd - 1) cwd, root)
                    | dirName -> (cwd @ [(dirName + "/")] , root)
                | ListDirectory lsCommand -> (cwd, applyLSCommand lsCommand cwd root)

            (newCwd, newRoot)
        let (_, root) = commands |> List.fold buildFileTree (["/"], { Name = "/"; Items = []})
        root

    let rec foldFileTree fFile fDir accum (fileTreeItem: FileTreeItem) =
        match fileTreeItem with
        | File file ->  fFile accum file
        | Directory dir -> 
            let localAccum = fDir accum dir
            List.fold (foldFileTree fFile fDir) localAccum dir.Items

    let calcFileTreeSize (fileTreeItem: FileTreeItem) =
        let sumFiles currSum file = currSum + file.Size
        let sumFolders currSum _ = currSum
        foldFileTree sumFiles sumFolders 0 fileTreeItem 

    let rec aggregateSizeList fileList (directory: DirectoryInfo) =
        fileList @ [calcFileTreeSize (Directory directory)]
    
    let buildSizeList (directory: DirectoryInfo) =
        (Directory directory)
        |> foldFileTree (fun lst _ -> lst) aggregateSizeList []

    let parseAndBuildSizeList input =
        input
        |> parseCommands
        |> buildRoot
        |> buildSizeList

    [<Solution(2022, 7, 1)>]
    let solve1 (input: string) =
        input
        |> parseAndBuildSizeList
        |> List.filter (fun size -> size < 100000)
        |> List.sum

    [<Solution(2022, 7, 2)>]
    let solve2 (input: string) =
        let sizeList = input |> parseAndBuildSizeList

        let diskSpace = 70000000
        let usedSpace = List.max sizeList 
        let currentlyUnused = diskSpace - usedSpace
        let requiredSpace = 30000000
        let amountToFreeUp = requiredSpace - currentlyUnused

        sizeList
        |> List.filter (fun size -> size > amountToFreeUp)
        |> List.min





