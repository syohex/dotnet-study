let findDifferentBinaryString (nums: string list) : string =
    let rec binaryStringToNumber (cs: char list) acc =
        match cs with
        | [] -> acc
        | h :: t -> binaryStringToNumber t (acc * 2 + (int h - int '0'))

    let set =
        nums
        |> List.map (fun s -> binaryStringToNumber (s |> Seq.toList) 0)
        |> Set.ofList

    let limit = int (System.Math.Pow(2, nums.Length))

    seq { 0 .. limit }
    |> Seq.find (fun n -> Set.contains n set |> not)
    |> sprintf "%0*B" nums.Length

// "00"
findDifferentBinaryString [ "01"; "10" ]

// "11"
findDifferentBinaryString [ "01"; "10" ]

// "10"
findDifferentBinaryString [ "00"; "01" ]

// "000"
findDifferentBinaryString [ "111"
                            "011"
                            "001" ]
