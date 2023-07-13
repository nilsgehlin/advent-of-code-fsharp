namespace AdventOfCode.Solutions.Day2

open System
open AdventOfCode.Common

module DomainTypes =
    type Action =
        | Rock
        | Paper
        | Scissor

    type Outcome =
        | ElfWin
        | Draw
        | PlayerWin

module Action =
    open DomainTypes
    let create symbol =
        match symbol with
        | 'A'
        | 'X' -> Rock

        | 'B'
        | 'Y' -> Paper

        | 'C'
        | 'Z' -> Scissor

        | _ -> raise (Exception("Exited due to invalid action symbol"))
    let whatBeats action =
        match action with
        | Rock -> Paper
        | Paper -> Scissor
        | Scissor -> Rock

    let whatIsBeatedBy action =
        match action with
        | Rock -> Scissor
        | Paper -> Rock
        | Scissor -> Paper

    let toScore action =
        match action with
        | Rock -> 1
        | Paper -> 2
        | Scissor -> 3

    let getActionScore action =
        match action with
        | Rock -> 1
        | Paper -> 2
        | Scissor -> 3

module Outcome = 
    open DomainTypes
    open Action
    let create symbol =
        match symbol with
        | 'X' -> ElfWin
        | 'Y' -> Draw
        | 'Z' -> PlayerWin
        | _ -> raise (Exception("Exited due to invalid outcome symbol"))
    
    let createFromActions actions =
        match actions with
        | (opponent, player) when opponent = player -> Draw
        | (opponent, player) when player = whatBeats (opponent) -> PlayerWin
        | _ -> ElfWin

    let toScore outcome =
        match outcome with
        | ElfWin -> 0
        | Draw -> 3
        | PlayerWin -> 6

    let getPlayerAction elfAction desiredOutcome =
        let playerAction =
            match desiredOutcome with
            | ElfWin -> whatIsBeatedBy elfAction
            | Draw -> elfAction
            | PlayerWin -> whatBeats elfAction
        playerAction

module Solution =
    let passAlong f input =
        f input
        input

    let validateInputLength input =
        if not (String.length input = 3) then
            raise (Exception "Exited due to: Invalid length of round input")

    let calculateRoundScoreOne input =
        input
        |> passAlong validateInputLength
        |> (fun input -> (Action.create input[0], Action.create input[2]))
        |> (fun (elfAction, playerAction) -> (Outcome.createFromActions (elfAction, playerAction), playerAction))
        |> (fun (outcome, playerAction) -> Outcome.toScore outcome + Action.toScore playerAction)

    let calculateRoundScoreTwo input =
        let (elfAction, desiredOutcome) =
            input
            |> passAlong validateInputLength
            |> (fun input -> (Action.create input[0], Outcome.create input[2]))

        let playerAction = Outcome.getPlayerAction elfAction desiredOutcome
        Outcome.toScore desiredOutcome + Action.toScore playerAction

    [<Solution(2022, 2, 1)>]
    let solve1 (input: string) =
        input
        |> Helpers.splitLines
        |> Array.map calculateRoundScoreOne
        |> Array.sum

    [<Solution(2022, 2, 2)>]
    let solve2 (input: string) =
        input
        |> Helpers.splitLines
        |> Array.map calculateRoundScoreTwo
        |> Array.sum
