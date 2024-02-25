let maxOperations (nums: int list) : int =
    let rec maxOperations' nums sum acc =
        match nums with
        | []
        | _ :: [] -> acc
        | h1 :: h2 :: t ->
            if h1 + h2 = sum then
                maxOperations' t sum (acc + 1)
            else
                acc

    match nums with
    | []
    | _ :: [] -> 0
    | h1 :: h2 :: t -> maxOperations' t (h1 + h2) 1

// 2
maxOperations [ 3; 2; 1; 4; 5 ]

// 1
maxOperations [ 3; 2; 6; 1; 2 ]
