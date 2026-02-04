let minimumPrefixLength (nums: int list) : int =
    let rec f nums prev =
        match nums with
        | [] -> 0
        | (i, h) :: t -> if h >= prev then i + 1 else f t h

    let nums = nums |> List.indexed |> List.rev

    match nums with
    | [] -> failwith "never reach here"
    | (_, h) :: t -> f t h

// 4
minimumPrefixLength [ 1; -1; 2; 3; 3; 4; 5 ]

// 3
minimumPrefixLength [ 4; 3; -2; -5 ]

// 0
minimumPrefixLength [ 1; 2; 3; 4 ]
