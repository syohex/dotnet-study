let findErrorNums (nums: int list) : (int * int) =
    let rec findErrorNums' nums duplicated acc =
        match nums with
        | [] -> duplicated, (Array.findIndex (fun n -> n = 0) acc) + 1
        | h :: t ->
            acc.[h - 1] <- acc.[h - 1] + 1

            if acc.[h - 1] = 2 then
                findErrorNums' t h acc
            else
                findErrorNums' t duplicated acc

    findErrorNums' nums -1 (Array.zeroCreate nums.Length)

// (2, 3)
findErrorNums [ 1; 2; 2; 4 ]

// (1, 2)
findErrorNums [ 1; 1 ]
