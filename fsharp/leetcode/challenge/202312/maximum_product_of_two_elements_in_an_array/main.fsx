let maxProduct (nums: int list) : int =
    nums
    |> List.sort
    |> List.rev
    |> List.take 2
    |> List.fold (fun acc n -> acc * (n - 1)) 1

// 12
maxProduct [ 3; 4; 5; 2 ]

// 16
maxProduct [ 1; 5; 4; 5 ]

// 12
maxProduct [ 3; 7 ]
