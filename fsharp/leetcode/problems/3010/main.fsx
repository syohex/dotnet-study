let minimumCost (nums: int list) : int =
    (List.head nums) + (nums |> List.skip 1 |> List.sort |> List.take 2 |> List.sum)

// 6
minimumCost [ 1; 2; 3; 12 ]

// 12
minimumCost [ 5; 4; 3 ]

// 12
minimumCost [ 10; 3; 1; 1 ]
