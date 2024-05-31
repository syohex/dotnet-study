let singleNumber (nums: int list) : (int * int) =
    let nums = nums |> List.map int64
    let x = nums |> List.fold (fun acc n -> acc ^^^ n) 0L
    let mask = x &&& -x

    let v =
        nums
        |> List.fold (fun acc n -> if (n &&& mask) <> 0L then acc ^^^ n else acc) 0L

    int v, int <| (v ^^^ x)

// [3, 5] or [5, 3]
singleNumber [ 1; 2; 1; 3; 2; 5 ]

// [-1,0] or [0, -1]
singleNumber [ -1; 0 ]

// [0,1] or [1,0]
singleNumber [ 1; 0 ]
