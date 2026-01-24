let minPairSum (nums: int list) : int =
    let nums = List.sort nums
    let limit = (List.length nums) / 2

    List.zip (List.take limit nums) (List.skip limit nums |> List.rev)
    |> List.map (fun (a, b) -> a + b)
    |> List.max

// 7
minPairSum [ 3; 5; 2; 3 ]

// 8
minPairSum [ 3; 5; 4; 2; 4; 6 ]
