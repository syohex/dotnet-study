let singleNumber (nums: int list) : int =
    let uniqueSum = nums |> Set.ofList |> Seq.fold (fun acc n -> acc + int64 n) 0L
    let sum = nums |> List.fold (fun acc n -> acc + int64 n) 0L

    (3L * uniqueSum - sum) / 2L |> int

// 3
singleNumber [ 2; 2; 3; 2 ]

// 99
singleNumber [ 0; 1; 0; 1; 0; 1; 99 ]
