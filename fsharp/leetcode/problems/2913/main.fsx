let sumCounts (nums: int list) : int =
    seq { 1 .. (List.length nums) }
    |> Seq.fold (fun acc i -> (List.windowed i nums) @ acc) []
    |> List.map (Set.ofList >> Set.count)
    |> List.map (fun n -> n * n)
    |> List.sum

// 15
sumCounts [ 1; 2; 1 ]

// 3
sumCounts [ 1; 1 ]

// 22
sumCounts [ 2; 2; 5; 5 ]
