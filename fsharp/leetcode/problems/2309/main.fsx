open System

let greatestLetter (s: string) : string =
    let rec findAnswer i (lowers: bool []) (uppers: bool []) =
        if i < 0 then
            ""
        elif lowers.[i] && uppers.[i] then
            (i + int 'A') |> char |> Char.ToString
        else
            findAnswer (i - 1) lowers uppers

    let rec greatestLetter' (cs: char list) lowers uppers =
        match cs with
        | [] -> findAnswer 25 lowers uppers
        | h :: t ->
            if Char.IsLower(h) then
                lowers.[(int h) - (int 'a')] <- true
            else
                uppers.[(int h) - (int 'A')] <- true

            greatestLetter' t lowers uppers

    let lowers: bool [] = Array.zeroCreate 26
    let uppers: bool [] = Array.zeroCreate 26

    greatestLetter' (s |> Seq.toList) lowers uppers


// E
greatestLetter "lEeTcOdE"

// R
greatestLetter "arRAzFif"

// ""
greatestLetter "AbCdEfGhIjK"
