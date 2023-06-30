namespace AdventOfCode.Solutions.Day5

open System
open AdventOfCode.Common

module Solution =
    type Crate = Crate of string
    type Stack = Stack of Crate list
    type MoveAction =
        {
            Quantity: int
            From: int
            To: int
        }
    
    let addToStack newItems (Stack items) = Stack(newItems @ items)

    let removeFromStack noCrates (Stack items) = 
        if List.length items < noCrates then
            failwith (sprintf "Not enough items in Stack, tried to remove %i crates but stack only has a size of %i" noCrates (List.length items))

        let removedItems = items[0 .. (noCrates - 1)]
        let newItems = List.removeManyAt 0 noCrates items
        (removedItems, Stack newItems)

    let peekStack (Stack items) =
        match items with
        | head :: tail -> head
        | [] ->  failwith "Tried to peek an empty stack"

    let createStacksFromDrawing drawing =
        let stackMap =
            let arr = Helpers.splitLines drawing
            arr[0..(Array.length arr - 2)]

        stackMap
        |> Array.map (Helpers.splitString "")
        |> Array.transpose
        |> Array.map (Helpers.joinString "")
        |> Helpers.takeArrayAt 2 4
        |> Array.map Helpers.trimWhitespace
        |> Array.map (Helpers.splitString "") 
        |> Array.map (fun arr -> arr[1..(Array.length arr - 2)])
        |> Array.map (Array.map Crate)
        |> Array.map Array.toList
        |> Array.map Stack
    
    let createMoveAction arr =
        match (Array.length arr) with
        | 3 -> { Quantity= arr[0]; From=arr[1]; To=arr[2]}
        | _ -> failwith (sprintf "Unvalid input array length when creating MoveAction, expected 3 but got %i" (Array.length arr) )

    let parseActions actionDesc =
        actionDesc
        |> Helpers.splitLines
        |> Array.map (Helpers.splitString " ")
        |> Array.map (Helpers.takeArrayAt 1 2)
        |> Array.map (Array.map int)
        |> Array.map createMoveAction

    let applyCrateMover reOrderingFunc (stacks: Stack array) (action: MoveAction) =
        let (poppedCrates, newStack) = removeFromStack action.Quantity stacks[action.From - 1]

        stacks
        |> Array.updateAt (action.From - 1) newStack
        |> (fun stackArr -> Array.updateAt (action.To - 1) (addToStack (reOrderingFunc poppedCrates) stackArr[action.To - 1]) stackArr)

    let parseInput input =
        let stackDrawing, actionDesc =
            let initialSplit = Helpers.splitString (Environment.NewLine + Environment.NewLine) input
            match Array.length initialSplit with
            | 2 -> (initialSplit[0], initialSplit[1])
            | _ -> failwith "Invalid input format"

        let actions = parseActions actionDesc
        let stacks = createStacksFromDrawing stackDrawing
        (stacks, actions)

    let getTopCrates stacks =
        stacks
        |> Array.map peekStack
        |> Array.map (fun (Crate crateString) -> crateString)
        |> (fun strArr -> String.Join ("", strArr))
    
    let applyCrateMover9000 = applyCrateMover List.rev
    let applyCrateMover9001 = applyCrateMover (fun stacks -> stacks)

    [<Solution(2022, 5, 1)>]
    let solve1 (input: string) =
        let (stacks, actions) = parseInput input

        actions
        |> Array.fold applyCrateMover9000 stacks
        |> getTopCrates

    [<Solution(2022, 5, 2)>]
    let solve2 (input: string) =
        let (stacks, actions) = parseInput input

        actions
        |> Array.fold applyCrateMover9001 stacks
        |> getTopCrates