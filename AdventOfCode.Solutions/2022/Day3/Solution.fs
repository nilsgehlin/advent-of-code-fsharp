namespace AdventOfCode.Solutions.Day3

open System
open AdventOfCode.Common

module Solution =
    type ItemType = { Priority: int }
    type Compartment = Compartment of ItemType list

    type Rucksack =
        { Compartments: (Compartment * Compartment) }

    type Group = Group of Rucksack array

    let createCompartment contents =
        let createItemType typeChar =
            if not (Char.IsAscii typeChar) then
                raise (Exception "Input character is not ASCII")

            match int typeChar with
            | prio when prio >= (int 'a') && prio <= (int 'z') -> { Priority = (int typeChar - int 'a' + 1) }
            | prio when prio >= (int 'A') && prio <= (int 'Z') ->
                { Priority = (int typeChar - int 'A' + int 'z' - int 'a' + 2) }
            | _ -> raise (Exception "Input character is not a-z OR A-Z")

        contents
        |> Helpers.splitString ""
        |> Array.toList
        |> List.removeAt 0
        |> (fun strList -> List.removeAt (List.length strList - 1) strList)
        |> List.map char
        |> List.map createItemType
        |> Compartment

    let createRucksack content =
        let splitInHalf str =
            if not ((String.length str) % 2 = 0) then
                raise (Exception "Rucksack input data is not divisible by two")

            let midIndex = (String.length str) / 2 - 1
            (str[0..midIndex], str[midIndex + 1 .. String.length str])

        content
        |> splitInHalf
        |> (fun (first, second) -> { Compartments = (createCompartment first, createCompartment second) })

    let createGroups rucksacks =
        let createGroup rucksackArray =
            match Array.length rucksackArray with
            | 3 -> Group rucksackArray
            | _ -> failwith "Invalid array length for a group"

        let noItems = Array.length rucksacks

        rucksacks |> Array.splitInto (noItems / 3) |> Array.map createGroup

    let findCommonItem (ruckSacks: ItemType list array) =
        let intersect =
            ruckSacks |> Array.map (fun itemLst -> Set.ofList itemLst) |> Set.intersectMany

        let noMatches = Set.count intersect

        match noMatches with
        | 1 -> (Set.toList intersect)[0]
        | _ -> failwith (sprintf "Expected 1 match, but got %i" noMatches)

    let findMisplacedItem (rucksack: Rucksack) =
        let (c1, c2) = rucksack.Compartments
        let (Compartment l1) = c1
        let (Compartment l2) = c2
        findCommonItem [| l1; l2 |]

    let findGroupBadge (Group rucksackArray) =
        let unpackRucksack rucksack =
            let (Compartment lst1) = fst rucksack.Compartments
            let (Compartment lst2) = snd rucksack.Compartments
            List.append lst1 lst2

        rucksackArray |> Array.map unpackRucksack |> findCommonItem

    let sumItems items =
        items |> Array.map (fun itemType -> itemType.Priority) |> Array.sum

    let inputToRucksacks input =
        input
        |> Helpers.splitLines
        |> Array.map createRucksack

    [<Solution(2022, 3, 1)>]
    let solve1 (input: string) =
        input
        |> inputToRucksacks
        |> Array.map findMisplacedItem
        |> sumItems

    [<Solution(2022, 3, 2)>]
    let solve2 (input: string) =
        input
        |> inputToRucksacks
        |> createGroups
        |> Array.map findGroupBadge
        |> sumItems
