namespace AdventOfCode.Solutions.Day11

open System
open System.Text.RegularExpressions
open AdventOfCode.Common

module Solution =
    type Item = { WorryLevel: int }

    type Operator =
        | Multiply
        | Add

    type Operation = { Operator: Operator; Operand: int}
    type Test = { Comparison: int; IfTrue: int; IfFalse: int }

    type Monkey =
        {
            Index: int
            StartingItems: Item list
            Operation: Operation
            Test: Test
        }

    let parseStartingItems line =
        let regex = new Regex(delimiter)
        regex.Split(str)


    let createMonkey input =
        let tmp =
            input
            |> Helpers.splitLines
            |> Array.iter (printfn "%s")
        
        printfn "------------------"
        ""

    [<Solution(2022, 11, 1)>]
    let solve1 (input: string) =
        input
        |> Helpers.splitString (sprintf "Monkey [0-9]:%s" Environment.NewLine)
        |> Array.removeAt 0
        |> Array.take 1
        |> Array.map createMonkey