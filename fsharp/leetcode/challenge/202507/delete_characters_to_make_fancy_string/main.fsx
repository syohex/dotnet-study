let makeFancyString (s: string) : string =
    s
    |> Seq.fold
        (fun ((acc: char list), prev1, prev2) c ->
            if c = prev1 && c = prev2 then
                acc, c, prev1
            else
                (c :: acc), c, prev1)
        ([], ' ', ' ')
    |> fun (acc, _, _) -> acc
    |> List.rev
    |> System.String.Concat

// leetcode
makeFancyString "leeetcode"

// aabaa
makeFancyString "aaabaaaa"

// aab
makeFancyString "aab"
