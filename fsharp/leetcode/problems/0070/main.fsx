let climbStairs (n: int) : int =
    let rec climbStairs' (pos: int) (n: int) : int =
        match pos with
        | _ when pos = n -> 1
        | _ when pos > n -> 0
        | _ ->
            (climbStairs' (pos + 1) n)
            + (climbStairs' (pos + 2) n)

    climbStairs' 0 n

climbStairs 2
climbStairs 3
climbStairs 10
