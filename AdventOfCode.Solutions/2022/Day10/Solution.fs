namespace AdventOfCode.Solutions.Day10

open AdventOfCode.Common
open System

module Solution =
    type Instruction =
        | Addx of value:int
        | Noop

    type PixelValue =
        | Lit
        | Dark

    let parseInstruction inputLine =
        match inputLine |> Seq.toList with
        | 'a' :: 'd' :: 'd' :: 'x' :: ' ' :: value -> Addx (Helpers.charListToInt value)
        | 'n' :: 'o' :: 'o' :: 'p' :: _ -> Noop
        | input -> failwith (sprintf "Invalid instruction %s" (Helpers.charListToString input))

    let incrPerCycle instruction =
        match instruction with
        | Addx value -> [| 0; value |]
        | Noop -> [| 0 |]

    let parseInput input =
        input
        |> Helpers.splitLines
        |> Array.map parseInstruction
    
    let registerValuePerCycle instructions =
        instructions 
        |> Array.map incrPerCycle
        |> Array.concat
        |> Array.scan (fun curr incr -> curr + incr) 1
        |> (fun arr -> Array.removeAt (Array.length arr - 1) arr)

    let calcSignalStrengths registerValues =
        registerValues 
        |> Array.indexed
        |> Helpers.takeArrayAt 19 40
        |> Array.map (fun (i, value) -> (i + 1) * value)

    let calcPixelValue crtIdx registerValue =
        let spritePositions = [registerValue - 1 .. 1 .. registerValue + 1]
        match List.tryFind (fun sp -> sp = crtIdx) spritePositions with
        | Some i -> Lit
        | None -> Dark

    let renderPixel pixel = 
        match pixel with
        | Lit -> "#"
        | Dark -> "."
    
    let renderImage image =
        image
        |> Array.map (Array.map renderPixel)
        |> Array.map (String.concat "")
        |> String.concat Environment.NewLine

    [<Solution(2022, 10, 1)>]
    let solve1 (input: string) =
        input
        |> parseInput
        |> registerValuePerCycle
        |> calcSignalStrengths
        |> Array.sum

    [<Solution(2022, 10, 2)>]
    let solve2 (input: string) =
        let crtDim = (6, 40)
        let crtRows = Array.replicate (fst crtDim) [| 0 .. (snd crtDim - 1) |]

        input
        |> parseInput
        |> registerValuePerCycle
        |> Array.take ((fst crtDim) * (snd crtDim))
        |> Array.chunkBySize (snd crtDim)
        |> Array.map2 (Array.map2 calcPixelValue) crtRows
        |> renderImage
        |> (fun str -> str |> Helpers.splitLines |> String.concat "")