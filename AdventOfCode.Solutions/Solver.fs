namespace AdventOfCode.Solutions

open System.Reflection
open System.IO
open System
open AdventOfCode.Common

type SolverArguments = {
    Year: int Option
    Day: int Option
    Part: int Option
}

module Solver =

    type DummyType = { x: int; }

    type SolutionInfo = {
        Year: int
        Day: int
        Part: int
        Method: MethodInfo
        InputFile: string
        ExampleFile: string
        OutputFile: string Option
    }

    type ResultType =
        | Success
        | Failure
        | Unknown

    let methodInfoToSolutionInfo (mi: MethodInfo) =
        let attr = mi.GetCustomAttribute(typeof<SolutionAttribute>, false) :?> SolutionAttribute
        let dataBasePath = sprintf "../data/%i/Day%i" attr.Year attr.Day

        let outputFileOpt =
            let outfilePath = (sprintf "%s.out" dataBasePath)
            match File.Exists outfilePath  with
            | true -> Some outfilePath
            | false -> None

        { 
            Year = attr.Year
            Day = attr.Day
            Part = attr.Part
            Method = mi
            InputFile = $"{dataBasePath}.in"
            ExampleFile= $"{dataBasePath}.example"
            OutputFile = outputFileOpt
        }

    let tryFindSolutionAttribute (mi: MethodInfo) =
        mi.CustomAttributes
        |> Seq.tryFind (fun attr -> attr.AttributeType = typeof<SolutionAttribute>)
        |> Option.map (fun _ -> mi)

    let getResultFlag resultType =
        match resultType with
        | Success -> Helpers.colorize Helpers.Green "+"
        | Failure -> Helpers.colorize Helpers.Red "-"
        | Unknown -> "?"

    let evaluateResult produced correct =
        match ($"{produced}" = correct) with
        | true -> getResultFlag Success
        | false -> sprintf "%s (should be %A)" (getResultFlag Failure) correct

    let readResult file =
        file |> File.ReadAllText |> Helpers.splitLines

    let printResult runExample solutionInfo =
        let inputFile =
            match runExample with
            | true -> solutionInfo.ExampleFile
            | false -> solutionInfo.InputFile

        let result = solutionInfo.Method.Invoke(null, [|inputFile |> File.ReadAllText|])
        let resultFlag = 
            match runExample with
            | true -> ""
            | false ->
                match solutionInfo.OutputFile with
                | None -> getResultFlag Unknown
                | Some outfile -> 
                    let correctValues = (readResult outfile)
                    match Array.tryItem (solutionInfo.Part - 1) correctValues with
                    | Some correct -> evaluateResult result correct 
                    | None -> getResultFlag Unknown
        
        printfn $"{solutionInfo.Year}: Day {solutionInfo.Day} Part {solutionInfo.Part} -> {result} {resultFlag}"

    let fieldFilter ref fieldValue =
        match fieldValue with
        | Some value -> ref = value
        | None -> true

    let runSolutions year day part runExample =
        let solutionsInAssembly =
            let types = Assembly.GetAssembly(typeof<DummyType>).GetTypes()
            types
            |> Array.collect (fun t -> t.GetMethods())
            |> Array.choose tryFindSolutionAttribute
            |> Array.map methodInfoToSolutionInfo
            |> Array.filter (fun si -> fieldFilter si.Year year)
            |> Array.filter (fun si -> fieldFilter si.Day day)
            |> Array.filter (fun si -> fieldFilter si.Part part)
            |> Array.sortBy (fun solInfo -> solInfo.Year, solInfo.Day, solInfo.Part)

        solutionsInAssembly
        |> Array.iter (fun solInfo -> printResult runExample solInfo)