let findEvenNumbers (digits: int list) : int list =
    let digits = List.toArray digits
    let len = digits.Length

    seq {
        for i in 0 .. (len - 1) do
            if digits.[i] <> 0 then
                for j in 0 .. (len - 1) do
                    if i <> j then
                        for k in 0 .. (len - 1) do
                            if i <> k && j <> k && digits.[k] % 2 = 0 then
                                yield digits.[i] * 100 + digits.[j] * 10 + digits.[k]
    }
    |> Set.ofSeq
    |> Set.toList
    |> List.sort

// [102,120,130,132,210,230,302,310,312,320]
findEvenNumbers [ 2; 1; 3; 0 ]

// [222,228,282,288,822,828,882]
findEvenNumbers [ 2; 2; 8; 8; 2 ]

// []
findEvenNumbers [ 3; 7; 5 ]
