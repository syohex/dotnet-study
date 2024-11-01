open System

let makeFancyString (s: string) : string =
    let rec makeFancyString' cs prev1 prev2 (acc: char list) =
        match cs with
        | [] -> List.rev acc |> String.Concat
        | h :: t ->
            if h = prev1 && h = prev2 then
                makeFancyString' t h prev1 acc
            else
                makeFancyString' t h prev1 (h :: acc)

    makeFancyString' (Seq.toList s) '?' '?' []

// "leetcode"
makeFancyString "leeetcode"

// "aabaa"
makeFancyString "aaabaaaa"

// "aab"
makeFancyString "aab"
