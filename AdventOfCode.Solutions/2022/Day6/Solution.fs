namespace AdventOfCode.Solutions.Day6

open AdventOfCode.Common

module Solution =
    let findNDistinctChars n input =
        let areAllCharsUnique chars =
            let distinctElements = List.distinct chars
            List.length chars = List.length distinctElements

        input
        |> Seq.toList
        |> List.windowed n
        |> List.findIndex areAllCharsUnique
        |> (+) n

    [<Solution(2022, 6, 1)>]
    let solve1 (input: string) =
        findNDistinctChars 4 input

    [<Solution(2022, 6, 2)>]
    let solve2 (input: string) =
        findNDistinctChars 14 input