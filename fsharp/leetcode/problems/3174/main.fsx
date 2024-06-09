let clearDigits (s: string) : string =
    let rec clearDigits' cs (acc: char list) =
        match cs with
        | [] -> acc |> List.rev |> System.String.Concat
        | h :: t ->
            if System.Char.IsDigit h then
                clearDigits' t (List.tail acc)
            else
                clearDigits' t (h :: acc)

    clearDigits' (Seq.toList s) []

// "abc"
clearDigits "abc"

// ""
clearDigits "a1b2"
