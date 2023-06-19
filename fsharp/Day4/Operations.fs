[<AutoOpen>]
module Day4.Operations

let createSection i =
    match i with
    | i when i > 0 -> Section i
    | _ -> failwith (sprintf "Invalid digit for a section, got %i" i)

let createSectionRange bounds =
    if not (Array.length bounds = 2) then
        failwith (sprintf "Expected bounds to be of size two, but got %i" (Array.length bounds))

    let rangeArr = [| for i in bounds[0].. bounds[1]-> createSection i |]
    SectionRange (Set.ofArray rangeArr)

let createPair (input: string array) =
    let getBounds (inStr: string) =
        inStr
        |> splitString "-"
        |> Array.map int
        
    let bounds1 = getBounds input[0]
    let bounds2 = getBounds input[1]
    Pair (createSectionRange bounds1, createSectionRange bounds2)

let applyToSections fn (Pair (range1, range2)) =
    let (SectionRange set1) = range1
    let (SectionRange set2) = range2
    fn set1 set2




        
