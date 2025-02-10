let clearDigits (s: string) : string =
    s
    |> Seq.fold
        (fun acc c ->
            if System.Char.IsAsciiDigit(c) then
                match acc with
                | [] -> []
                | _ :: t -> t
            else
                c :: acc)
        []
    |> List.rev
    |> System.String.Concat

// abc
clearDigits "abc"

// ""
clearDigits "a1b2"

// z
clearDigits "za1b2"

// ""
clearDigits "123"
