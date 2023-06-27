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
        OutputFile: string Option
    }

    type Color =
        | Green
        | Red

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
            OutputFile = outputFileOpt
        }

    let tryFindSolutionAttribute (mi: MethodInfo) =
        mi.CustomAttributes
        |> Seq.tryFind (fun attr -> attr.AttributeType = typeof<SolutionAttribute>)
        |> Option.map (fun _ -> mi)

    let colorize color str =
        let esc = string (char 0x1B)
        let colorCode =
            match color with
            | Green -> "32"
            | Red -> "31"
        esc + "[" + colorCode + ";1m" + str + esc + "[0m"

    let getResultFlag resultType =
        match resultType with
        | Success -> colorize Green "+"
        | Failure -> colorize Red "-"
        | Unknown -> "?"

    let evaluateResult produced correct =
        match (produced = correct) with
        | true -> getResultFlag Success
        | false -> sprintf "%s (should be %i)" (getResultFlag Failure) correct

    let readResult partNo file =
        file |> File.ReadAllText |> Helpers.splitLines |> Array.map int

    let printResult solutionInfo =
        let result = solutionInfo.Method.Invoke(null, [|solutionInfo.InputFile |> File.ReadAllText|])
        let result = downcast result : int
        let resultFlag = 
            match solutionInfo.OutputFile with
            | None -> getResultFlag Unknown
            | Some outfile -> 
                let correctValues = (readResult solutionInfo.Part outfile)
                match Array.tryItem (solutionInfo.Part - 1) correctValues with
                | Some correct -> evaluateResult result correct 
                | None -> getResultFlag Unknown
        
        printfn $"{solutionInfo.Year}: Day {solutionInfo.Day} Part {solutionInfo.Part} -> {result} {resultFlag}"

    let fieldFilter ref fieldValue =
        match fieldValue with
        | Some value -> ref = value
        | None -> true

    let runSolutions year day part =
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
        |> Array.iter (fun solInfo -> printResult solInfo)