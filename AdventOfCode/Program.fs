open AdventOfCode.Solutions
open System
open Argu

type Arguments =
    | Year of int
    | Day of int
    | Part of int
    | Example of bool

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Year _ -> "specify year"
            | Day _ -> "specify day"
            | Part _ -> "specify part"
            | Example _ -> "give example flag"

[<EntryPoint>]
let main args =
    let parser = ArgumentParser.Create<Arguments>()
    let results = parser.ParseCommandLine (inputs = args)

    let runExample =
        match results.TryGetResult Arguments.Example with
        | Some b -> b
        | None -> false

    Solver.runSolutions (results.TryGetResult Arguments.Year) (results.TryGetResult Arguments.Day) (results.TryGetResult Arguments.Part) runExample
    0