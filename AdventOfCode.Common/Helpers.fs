namespace AdventOfCode.Common

open System.Text.RegularExpressions
open System

type SolutionAttribute(year: int, day: int, part: int) =
    inherit System.Attribute()
    member val Year: int = year with get
    member val Day: int = day with get
    member val Part: int = part with get

module Helpers =
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

