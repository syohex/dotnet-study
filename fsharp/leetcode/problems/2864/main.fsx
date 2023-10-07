open System

let maximumOddBinaryNumber (s: string) : string =
    let ones = s |> Seq.filter ((=) '1') |> Seq.length
    let zeros = s.Length - ones

    (List.init (ones - 1) (fun _ -> '1'))
    @ (List.init zeros (fun _ -> '0'))
    @ [ '1' ]
    |> String.Concat

// "001"
maximumOddBinaryNumber "010"

// "1001"
maximumOddBinaryNumber "0101"

// "101"
maximumOddBinaryNumber "011"

// "11001"
maximumOddBinaryNumber "00111"
