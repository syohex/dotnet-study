let maximumOddBinaryNumber (s: string) : string =
    let zeros, ones =
        s
        |> Seq.fold (fun (zeros, ones) c -> if c = '0' then zeros + 1, ones else zeros, ones + 1) (0, 0)

    (List.init (ones - 1) (fun _ -> '1'))
    @ (List.init zeros (fun _ -> '0'))
    @ [ '1' ]
    |> System.String.Concat

// "001"
maximumOddBinaryNumber "010"

// "1001"
maximumOddBinaryNumber "0101"
