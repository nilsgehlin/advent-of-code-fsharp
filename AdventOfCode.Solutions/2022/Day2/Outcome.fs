namespace AdventOfCode.Solutions.Day2

open System
open DomainTypes
open Action

module Outcome = 
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