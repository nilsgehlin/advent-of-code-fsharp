namespace AdventOfCode.Solutions.Day2

module DomainTypes =
    type Action =
        | Rock
        | Paper
        | Scissor

    type Outcome =
        | ElfWin
        | Draw
        | PlayerWin