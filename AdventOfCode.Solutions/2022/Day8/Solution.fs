namespace AdventOfCode.Solutions.Day8

open AdventOfCode.Common

module Solution =

    type Direction =
        | Left
        | Right
        | Top
        | Bottom

    let compareToCurrentMax currMax height =
        if height > currMax then
            (true, height)
        else
            (false, currMax)

    let calcRowVisibility heights =
        let (visibilityMap, _) = heights |> Array.mapFold compareToCurrentMax -1
        visibilityMap
    
    let mirror arr = arr |> Array.map (Array.rev)
    let transpose arr = arr |> Array.transpose

    let rowTransformation dir =
        let rowTransformation a =
            match dir with
            | Left -> a
            | Right -> mirror a
            | Top -> transpose a
            | Bottom -> a |> transpose |> mirror
        rowTransformation

    let inverseRowTransformation dir =
        let inverseRowTransformation a =
            match dir with
            | Bottom -> a |> mirror |> transpose
            | dir -> rowTransformation dir a
        inverseRowTransformation

    let createDirectionVisibilityMap heightMap direction =
        heightMap
        |> rowTransformation direction
        |> Array.map calcRowVisibility
        |> inverseRowTransformation direction

    let createVisibilityMap (directionMaps: bool array array array) =
        directionMaps |> Seq.reduce (Array.map2 (Array.map2 (fun b1 b2 -> b1 || b2)))

    let parseHeightMap input =
        input
        |> Helpers.splitLines
        |> Array.map Seq.toArray
        |> Array.map (Array.map (string >> int))

    let calcViewingDistance distances =
        match Array.tryHead distances with
        | Some d when d <= 1 ->
            distances
            |> Array.pairwise
            |> Array.map (fun (e1, e2) -> e2 - e1)
            |> Array.takeWhile (fun delta -> delta <= 1)
            |> Array.length
        | _ -> 0

    let rec getLineOfSight refHeight line =
        match line with
        | head :: tail ->
            match head with
            | h when h < refHeight -> h :: getLineOfSight refHeight tail
            | h -> [h]
        | [] -> []


    let dirScenicScore (row: int) (col: int) (heightMap: int array array) direction =
        let noCols = Array.length heightMap[row]
        let noRows = Array.length heightMap

        let line =
            let fullLine =
                match direction with
                | Left | Right -> heightMap[row]
                | Top | Bottom -> (transpose heightMap)[col]

            match direction with
            | Left -> fullLine[0 .. col] |> Array.rev
            | Top -> fullLine[0 .. row] |> Array.rev
            | Right -> fullLine[col .. noCols - 1]
            | Bottom -> fullLine[row .. noRows - 1]

        let refHeight = Array.head line
        line |> Array.tail |> Array.toList |> getLineOfSight refHeight |> List.length

    let calculateScenicScore (row: int) (col: int) (heightMap: int array array) =
        let tmp = 
            [|Top; Left; Bottom; Right|]
            |> Array.map (dirScenicScore row col heightMap)
        tmp
        |> Array.reduce (fun s1 s2 -> s1 * s2)

    let calculateScenicScoreMap heightMap =
        heightMap
        |> Array.mapi (fun ri r -> Array.mapi (fun ci _ -> calculateScenicScore ri ci heightMap) r)


    [<Solution(2022, 8, 1)>]
    let solve1 (input: string) =
        let heightMap = input |> parseHeightMap

        [| Left; Right; Top; Bottom |]
        |> Array.map (createDirectionVisibilityMap heightMap)
        |> createVisibilityMap
        |> Array.sumBy (Array.sumBy (fun b -> if b then 1 else 0))

    [<Solution(2022, 8, 2)>]
    let solve2 (input: string) =
        input |> parseHeightMap |> calculateScenicScoreMap |> Array.concat |> Array.max