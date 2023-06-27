namespace AdventOfCode.Solutions.Day1

open System
open AdventOfCode.Common

module Solution =
    let calculateCalories =
        Helpers.splitString Environment.NewLine >> Array.map int >> Array.sum

    let processInput rawInput =
        rawInput |> Helpers.splitString (Environment.NewLine + Environment.NewLine)

    let calcCaloriesPerElf input =
        input |> processInput |> Array.map calculateCalories

    [<Solution(2022, 1, 1)>]
    let solve1 (input: string) =
        input |> calcCaloriesPerElf |> Array.max

    [<Solution(2022, 1, 2)>]
    let solve2 (input: string) =
        input |> calcCaloriesPerElf |> Array.sortDescending |> Array.take 3 |> Array.sum
