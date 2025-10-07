let alternatingSum (nums: int list) : int =
    nums
    |> List.indexed
    |> List.fold (fun acc (i, n) -> if i % 2 = 0 then acc + n else acc - n) 0

// -4
alternatingSum [ 1; 3; 5; 7 ]

// 100
alternatingSum [ 100 ]
