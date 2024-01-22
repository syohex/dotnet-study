let findErrorNums (nums: int list) : int * int =
    let len = List.length nums

    let rec findErrorNums' nums prev dup miss =
        match nums with
        | [] -> dup, (if miss = -1 then len else miss)
        | h :: t ->
            if h = prev then findErrorNums' t h h miss
            elif h = prev + 2 then findErrorNums' t h dup (h - 1)
            else findErrorNums' t h dup miss

    match List.sort nums with
    | [] -> failwith "never reach here"
    | h :: t -> findErrorNums' t h -1 (if h = 1 then -1 else 1)

// (2, 3)
findErrorNums [ 1; 2; 2; 4 ]

// (1, 2)
findErrorNums [ 1; 1 ]

// (2, 1)
findErrorNums [ 2; 2; 3; 4 ]

// (3, 4)
findErrorNums [ 1; 2; 3; 3 ]
