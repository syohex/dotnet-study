let climbStairs (n: int) : int =
    let rec climbStairs' m n prev2 prev1 =
        if m = n then
            prev1 + prev2
        else
            climbStairs' (m + 1) n prev1 (prev1 + prev2)

    match n with
    | 1 -> 1
    | 2 -> 2
    | _ -> climbStairs' 3 n 1 2

// 1
climbStairs 1

// 2
climbStairs 2

// 3
climbStairs 3

// 63245986
climbStairs 38
