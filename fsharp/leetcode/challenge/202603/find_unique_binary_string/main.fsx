let findDifferentBinaryString (nums: string list) : string =
    nums
    |> List.mapi (fun i s -> if s.[i] = '1' then '0' else '1')
    |> System.String.Concat

// "11"
findDifferentBinaryString [ "01"; "10" ]

// "10"
findDifferentBinaryString [ "00"; "01" ]

// "000"
findDifferentBinaryString [ "111"; "011"; "001" ]
