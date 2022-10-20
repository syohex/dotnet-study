let intToRoman (num: int) : string =
    let intToRoman' num baseNum tenChar fiveChar oneChar acc =
        let digit = num / baseNum

        match digit with
        | 9 -> tenChar :: oneChar :: acc
        | 5
        | 6
        | 7
        | 8 ->
            seq { 1 .. (digit - 5) }
            |> Seq.fold (fun acc _ -> oneChar :: acc) (fiveChar :: acc)
        | 4 -> fiveChar :: oneChar :: acc
        | 0
        | 1
        | 2
        | 3 ->
            seq { 1..digit }
            |> Seq.fold (fun acc _ -> oneChar :: acc) acc
        | _ -> failwith "never reach here"

    intToRoman' (num % 10000) 1000 '?' '!' 'M' []
    |> intToRoman' (num % 1000) 100 'M' 'D' 'C'
    |> intToRoman' (num % 100) 10 'C' 'L' 'X'
    |> intToRoman' (num % 10) 1 'X' 'V' 'I'
    |> List.rev
    |> System.String.Concat


// "III"
intToRoman 3

// "LVIII"
intToRoman 58

// "MCMXCIV"
intToRoman 1994
