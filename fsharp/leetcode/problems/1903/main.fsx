open System

let largestOddNumber (num: string) : string =
    let rec largestOddNumber' (cs: char list) =
        match cs with
        | [] -> ""
        | h :: t ->
            let n = int h - int '0'

            if n % 2 = 1 then
                cs |> List.rev |> String.Concat
            else
                largestOddNumber' t

    largestOddNumber' (num |> Seq.rev |> Seq.toList)

// 5
largestOddNumber "52"

// ""
largestOddNumber "4206"

// "35427"
largestOddNumber "35427"

// "1234567890123456789"
largestOddNumber "1234567890123456789"
