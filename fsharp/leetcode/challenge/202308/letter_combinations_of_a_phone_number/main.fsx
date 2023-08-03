open System

let letterCombinations (digits: string) : string list =
    let table = [| "abc"; "def"; "ghi"; "jkl"; "mno"; "pqrs"; "tuv"; "wxyz" |]

    let rec letterCombinations' digits (acc: char list) ret =
        match digits with
        | [] ->
            let s = acc |> List.rev |> String.Concat
            s :: ret
        | h :: t ->
            let index = int h - int '2'

            table.[index]
            |> Seq.fold (fun ret c -> letterCombinations' t (c :: acc) ret) ret

    if digits.Length = 0 then
        []
    else
        letterCombinations' (Seq.toList digits) [] []

// ["ad","ae","af","bd","be","bf","cd","ce","cf"]
letterCombinations "23" |> List.rev |> printfn "%A"

// []
letterCombinations "" |> printfn "%A"

// ["a", "b", "c"]
letterCombinations "2" |> List.rev |> printfn "%A"
