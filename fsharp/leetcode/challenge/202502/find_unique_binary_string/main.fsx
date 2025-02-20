let findDifferentBinaryString (nums: string list) : string =
    nums
    |> List.indexed
    |> List.fold (fun (acc: char list) (i, s) -> if s.[i] = '0' then '1' :: acc else '0' :: acc) []
    |> List.rev
    |> System.String.Concat

// "11"
findDifferentBinaryString [ "01"; "10" ]

// "10"
findDifferentBinaryString [ "00"; "01" ]

// "000"
findDifferentBinaryString [ "111"; "011"; "001" ]
