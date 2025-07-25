let maxSum (nums: int list) : int =
    let s = Set.ofList nums

    if Set.forall (fun n -> n < 0) s then
        Set.maxElement s
    else
        s |> Set.filter (fun n -> n > 0) |> Set.fold (+) 0

// 15
maxSum [ 1; 2; 3; 4; 5 ]

// 1
maxSum [ 1; 1; 0; 1; 1 ]

// 3
maxSum [ 1; 2; -1; -2; 1; 0; -1 ]

// 20
maxSum [ -20; 20 ]

// -100
maxSum [ -100 ]

// -50
maxSum [ -100; -50 ]
