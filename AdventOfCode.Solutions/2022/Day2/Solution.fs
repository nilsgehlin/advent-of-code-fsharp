namespace AdventOfCode.Solutions.Day2

open System
open AdventOfCode.Common
open DomainTypes

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
