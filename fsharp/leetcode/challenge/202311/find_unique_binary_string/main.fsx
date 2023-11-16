open System

let findDifferentBinaryString (nums: string list) : string =
    let rec findDifferenceBinaryString' (nums: string list) i (acc: char list) =
        match nums with
        | [] -> acc |> List.rev |> String.Concat
        | h :: t ->
            if h.[i] = '0' then
                findDifferenceBinaryString' t (i + 1) ('1' :: acc)
            else
                findDifferenceBinaryString' t (i + 1) ('0' :: acc)

    findDifferenceBinaryString' nums 0 []

// 11
findDifferentBinaryString [ "01"; "10" ]

// 10
findDifferentBinaryString [ "00"; "01" ]

// 000
findDifferentBinaryString [ "111"; "011"; "001" ]

// 0100
findDifferentBinaryString [ "1001"; "1000"; "0110"; "1111" ]

// 111
findDifferentBinaryString [ "000"; "001"; "110" ]
