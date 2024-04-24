let tribonacci (n: int) : int =
    let rec tribonacci' i n prev1 prev2 prev3 =
        if i > n then
            prev1
        else
            tribonacci' (i + 1) n (prev1 + prev2 + prev3) prev1 prev2

    match n with
    | 0 -> 0
    | 1
    | 2 -> 1
    | _ -> tribonacci' 3 n 1 1 0

// 4
tribonacci 4

// 1389537
tribonacci 25
