let minOperations (nums: int list) : int =
    let rec keepMonotonicIncreasing num stack =
        match stack with
        | [] -> []
        | top :: t -> if top > num then keepMonotonicIncreasing num t else stack

    let rec minOperations' nums stack acc =
        match nums with
        | [] -> acc
        | num :: t ->
            let stack' = keepMonotonicIncreasing num stack

            if num = 0 then
                minOperations' t stack' acc
            else
                match stack' with
                | [] -> minOperations' t [ num ] (acc + 1)
                | top :: _ ->
                    if num <> top then
                        minOperations' t (num :: stack') (acc + 1)
                    else
                        minOperations' t stack' acc

    minOperations' nums [] 0

// 1
minOperations [ 0; 2 ]

// 3
minOperations [ 3; 1; 2; 1 ]

// 4
minOperations [ 1; 2; 1; 2; 1; 2 ]
