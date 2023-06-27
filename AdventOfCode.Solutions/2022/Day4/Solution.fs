namespace AdventOfCode.Solutions.Day4

open System
open AdventOfCode.Common

module Solution =
    type Section = Section of int
    type SectionRange = SectionRange of Section Set
    type Pair = Pair of (SectionRange * SectionRange)

    let createSection i =
        match i with
        | i when i > 0 -> Section i
        | _ -> failwith (sprintf "Invalid digit for a section, got %i" i)

    let createSectionRange bounds =
        if not (Array.length bounds = 2) then
            failwith (sprintf "Expected bounds to be of size two, but got %i" (Array.length bounds))

        let rangeArr = [| for i in bounds[0] .. bounds[1] -> createSection i |]
        SectionRange(Set.ofArray rangeArr)

    let createPair (input: string array) =
        let getBounds (inStr: string) =
            inStr |> Helpers.splitString "-" |> Array.map int

        let bounds1 = getBounds input[0]
        let bounds2 = getBounds input[1]
        Pair(createSectionRange bounds1, createSectionRange bounds2)

    let applyToSections fn (Pair(range1, range2)) =
        let (SectionRange set1) = range1
        let (SectionRange set2) = range2
        fn set1 set2

    let pairContainsSubset set1 set2 =
        (Set.isSubset set1 set2) || (Set.isSubset set2 set1)

    let countSectionFilter filter pairs =
        pairs |> Array.where (applyToSections filter) |> Array.length

    let pairIntersects set1 set2 =
        let interesectCount = Set.intersect set1 set2 |> Set.count
        interesectCount > 0

    let inputToPairs input =
        input
        |> Helpers.splitString Environment.NewLine
        |> Array.map (Helpers.splitString ",")
        |> Array.map createPair

    [<Solution(2022, 4, 1)>]
    let solve1 (input: string) =
        input
        |> inputToPairs
        |> countSectionFilter pairContainsSubset

    [<Solution(2022, 4, 2)>]
    let solve2 (input: string) =
        input
        |> inputToPairs
        |> countSectionFilter pairIntersects
