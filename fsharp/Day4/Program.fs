open System
open System.IO
open Day4

let pairContainsSubset set1 set2 =
    (Set.isSubset set1 set2) || (Set.isSubset set2 set1)
let pairs =
    "input.txt"
    |> File.ReadAllText
    |> splitString Environment.NewLine
    |> Array.map (splitString ",")
    |> Array.map createPair

let countSectionFilter filter =
    pairs
    |> Array.where (applyToSections filter)
    |> Array.length

let pairIntersects set1 set2 =
    let interesectCount = Set.intersect set1 set2 |> Set.count
    interesectCount > 0

let answerPartOne = countSectionFilter pairContainsSubset
let answerPartTwo= countSectionFilter pairIntersects

printf "Answer part one: %i%s" answerPartOne Environment.NewLine
printf "Answer part two: %i%s" answerPartTwo Environment.NewLine