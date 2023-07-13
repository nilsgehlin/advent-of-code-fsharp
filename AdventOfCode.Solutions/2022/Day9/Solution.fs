namespace AdventOfCode.Solutions.Day9

open AdventOfCode.Common

module Solution =

    type DirectionType =
        | Up
        | Down
        | Right
        | Left
 
    type Command =
        {
            Direction: DirectionType
            Length: int
        }

    type Knot =
        {
            Coord: (int * int)
            Follower: Knot option
        }

    let buildRope length =
        let rec createKnots length =
            match length with
            | 0 -> None
            | _ -> Some { Coord = (0, 0); Follower = createKnots (length - 1)}

        match createKnots length with
        | Some head -> head
        | None -> failwith "There is not head"

    let rec ropeIter f knot =
        f knot
        match knot.Follower with
        | Some follower -> ropeIter f follower
        | None -> ()

    let rec ropeFold folder state knot =
        let newState = folder state knot
        match knot.Follower with
        | Some follower -> ropeFold folder newState follower
        | None -> newState
    
    let ropeTail knot =
        ropeFold (fun _ knot -> knot) knot knot
    
    let rec updateKnotAt i coord knot =
        match i with
        | 0 -> { knot with Coord = coord}
        | _ ->
            match knot.Follower with
            | Some follower -> { knot with Follower = Some (updateKnotAt (i - 1) coord follower)}
            | None -> failwith "Rope index out of range"

    let parseCommand line =
        match line |> Seq.toList with
        | dir :: ' ' :: length ->
            let l = Helpers.charListToInt length
            match dir with
            | 'U' -> {Direction = Up; Length = l}
            | 'D' -> {Direction = Down; Length = l}
            | 'R' -> {Direction = Right; Length = l}
            | 'L' -> {Direction = Left; Length = l}
            | c -> failwith (sprintf "Invalid command direction %c" c)
        | s -> failwith (sprintf "Invalid input format for command, got %s" (s |> string))

    let moveHead state direction =
        match direction with
        | Up -> match state.Coord with (x, y) -> x, y + 1
        | Down -> match state.Coord with (x, y) -> x, y - 1
        | Right -> match state.Coord with (x, y) -> x + 1, y
        | Left -> match state.Coord with (x, y) -> x - 1, y

    let isTouching dx dy = abs dx <= 1 && abs dy <= 1
    let sign i = i / (max (abs i) 1)

    let calcTailMove dx dy =
        if isTouching dx dy then
            (0, 0)
        else
            (sign dx, sign dy)

    let rec moveFollower (follower: Knot) (newHeadCoord: (int * int)) =
        let dx, dy = (fst newHeadCoord - fst follower.Coord, snd newHeadCoord - snd follower.Coord)
        let movex, movey = calcTailMove dx dy
        let newFollowerCoord = (fst follower.Coord + movex, snd follower.Coord + movey)

        let newFollower = { Coord = newFollowerCoord; Follower = None }

        match follower.Follower with
        | Some f -> Some { newFollower with Follower = moveFollower f newFollowerCoord }
        | None -> Some newFollower

    let moveRope head direction =
        let newHeadCoord = moveHead head direction
        let follower =
            match head.Follower with
            | Some f -> f
            | None -> failwith "Expected head to have at least one follower"

        { Coord = newHeadCoord; Follower = moveFollower follower newHeadCoord }
        
    let parseCommands input =
        input
        |> Helpers.splitLines
        |> Array.map parseCommand
        |> Array.map (fun cmd -> Array.replicate cmd.Length cmd.Direction)
        |> Array.concat

    let findDistinctTailPositions ropes =
        ropes 
        |> Array.map (fun head -> (ropeTail head).Coord)
        |> Array.distinct
        |> Array.length

    [<Solution(2022, 9, 1)>]
    let solve1 (input: string) =
        input
        |> parseCommands
        |> Array.scan moveRope (buildRope 2)
        |> findDistinctTailPositions

    [<Solution(2022, 9, 2)>]
    let solve2 (input: string) =
        input
        |> parseCommands
        |> Array.scan moveRope (buildRope 10)
        |> findDistinctTailPositions