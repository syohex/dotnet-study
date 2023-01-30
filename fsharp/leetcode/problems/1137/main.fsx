let tribonacci (n: int) : int =
    let rec tribonacci' i n prev3 prev2 prev1 =
        if i > n then
            prev1
        else
            tribonacci' (i + 1) n prev2 prev1 (prev3 + prev2 + prev1)

    match n with
    | 0 -> 0
    | 1
    | 2 -> 1
    | _ -> tribonacci' 3 n 0 1 1

// 4
tribonacci 4

// 1389537
tribonacci 25

// 0
tribonacci 0

// 1
tribonacci 1

// 1
tribonacci 1
