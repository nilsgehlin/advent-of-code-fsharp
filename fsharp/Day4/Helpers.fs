[<AutoOpen>]
module Day4.Helpers

open System.Text.RegularExpressions

let splitString delimiter str =
    let regex = new Regex(delimiter)
    regex.Split(str)