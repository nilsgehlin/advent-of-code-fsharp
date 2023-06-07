open System
open System.IO
open System.Text.RegularExpressions

type Shape =
    | Rock
    | Paper
    | Scissor

type Outcome =
    | Loose
    | Draw
    | Win

let CreateShape symbol =
    match symbol with
    | 'A'
    | 'X' -> Shape.Rock

    | 'B'
    | 'Y' -> Shape.Paper

    | 'C'
    | 'Z' -> Shape.Scissor

    | _ -> raise (Exception ("Exited due to invalid shape symbol"))

let CreateOutcome symbol =
    match symbol with
    | 'X' -> Outcome.Loose
    | 'Y' -> Outcome.Draw
    | 'Z' -> Outcome.Win
    | _ -> raise (Exception ("Exited due to invalid outcome symbol"))

let passAlong f input =
    f input
    input

let validateInputLength input =
    if not (String.length input = 3) then raise (Exception "Exited due to: Invalid length of round input")

let whatBeats shape =
    match shape with
    | Shape.Rock -> Shape.Paper
    | Shape.Paper -> Shape.Scissor
    | Shape.Scissor -> Shape.Rock

let whatIsBeatedBy shape =
    match shape with
    | Shape.Rock -> Shape.Scissor
    | Shape.Paper -> Shape.Rock
    | Shape.Scissor -> Shape.Paper

let getOutcomeScore shapes =
    match shapes with
    | (opponent, player) when opponent = player -> 3
    | (opponent, player) when player = whatBeats(opponent) -> 6
    | _ -> 0

let getShapeScore shape =
    match shape with
    | Shape.Rock -> 1
    | Shape.Paper -> 2
    | Shape.Scissor-> 3

let getPlayerActions (opponent, outcome) =
    let player =
        match outcome with
        | Outcome.Loose -> whatIsBeatedBy opponent
        | Outcome.Draw -> opponent
        | Outcome.Win -> whatBeats opponent
    (opponent, player)

let calculateRoundScoreOne input =
    input
    |> passAlong validateInputLength
    |> (fun input -> (CreateShape input[0], CreateShape input[2]))
    |> (fun (opponent, player) -> getOutcomeScore (opponent, player) + getShapeScore player)

let calculateRoundScoreTwo input =
    input
    |> passAlong validateInputLength
    |> (fun input -> (CreateShape input[0], CreateOutcome input[2]))
    |> getPlayerActions 
    |> (fun (opponent, player) -> getOutcomeScore (opponent, player) + getShapeScore player)

let splitString delimiter str =
    let regex = new Regex(delimiter)
    regex.Split(str)

let rounds = "input.txt" |> File.ReadAllText |> splitString Environment.NewLine

let answerPartOne = rounds |> Array.map calculateRoundScoreOne |> Array.sum
printf "Answer part one: %i%s" answerPartOne Environment.NewLine

let answerPartTwo = rounds |> Array.map calculateRoundScoreTwo |> Array.sum
printf "Answer part two: %i%s" answerPartTwo Environment.NewLine