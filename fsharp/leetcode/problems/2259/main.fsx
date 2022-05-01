let removeDigit (number: string) (digit: char) : string =
    let cs = number |> Seq.toList

    cs
    |> List.mapi (fun i c -> i, c)
    |> List.fold
        (fun acc (i, c) ->
            if c = digit then
                let cand = cs |> List.removeAt i |> System.String.Concat
                cand :: acc
            else
                acc)
        []
    |> List.sort
    |> List.rev
    |> List.head

// "12"
removeDigit "123" '3'

// "231"
removeDigit "1231" '1'

// "423"
removeDigit "4234" '4'

// "51"
removeDigit "551" '5'
