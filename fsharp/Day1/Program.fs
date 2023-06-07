open System
open System.IO
open System.Text.RegularExpressions

let SplitString delimiter (str: string) =
    let regex = new Regex(delimiter)
    regex.Split(str)

let input = "input.txt" |> File.ReadAllText

let calculateCalories =
    SplitString Environment.NewLine >> Array.map int >> Array.sum

let caloriesPerElf =
    input
    |> SplitString(Environment.NewLine + Environment.NewLine)
    |> Array.map calculateCalories

let answerPartOne = caloriesPerElf |> Array.max

let answerPartTwo =
    caloriesPerElf |> Array.sortDescending |> Array.take 3 |> Array.sum

printf "Answer part one: %i%s" answerPartOne Environment.NewLine
printf "Answer part two: %i%s" answerPartTwo Environment.NewLine
