namespace AdventOfCode.Solutions.Day2

open System
open DomainTypes

module Action =
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