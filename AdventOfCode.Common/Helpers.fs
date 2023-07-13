namespace AdventOfCode.Common

open System.Text.RegularExpressions
open System

type SolutionAttribute(year: int, day: int, part: int) =
    inherit System.Attribute()
    member val Year: int = year with get
    member val Day: int = day with get
    member val Part: int = part with get

module Helpers =

    type Color =
        | Green
        | Red
    let colorize color str =
        let esc = string (char 0x1B)
        let colorCode =
            match color with
            | Green -> "32"
            | Red -> "31"
        esc + "[" + colorCode + ";1m" + str + esc + "[0m"


    let splitString delimiter (str: string) =
        let regex = new Regex(delimiter)
        regex.Split(str)

    let splitLines input =
        splitString Environment.NewLine input

    let joinString (delim: string) (arr: string array) = String.Join(delim, arr)

    let trimWhitespace (str: string) =
        str.Trim()

    let takeArrayAt start incr arr =
        let sq = seq { for i in start..incr .. (Array.length arr) -> arr[i] }
        Seq.toArray sq

    let charListToString charLst = charLst |> List.map (fun c -> string c) |> String.concat "" 
    let charListToInt charLst =  charLst |> charListToString |> int

