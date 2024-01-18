let climbStairs (n: int) : int =
    let rec climbStairs' i n prev1 prev2 =
        if i >= n then
            prev1 + prev2
        else
            climbStairs' (i + 1) n (prev1 + prev2) prev1

    match n with
    | 1 -> 1
    | _ -> climbStairs' 2 n 1 1

// 2
climbStairs 2

// 3
climbStairs 3

// 5
climbStairs 4

climbStairs 45
