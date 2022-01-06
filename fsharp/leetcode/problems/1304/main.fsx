let sumZero (n: int) : int list =
    let rec sumZero' (n: int) (m: int) (acc: int list) : int list =
        match n with
        | 0 -> acc
        | 1 -> 0 :: acc
        | _ -> sumZero' (n - 2) (m - 1) (-m :: m :: acc)

    sumZero' n (n / 2) []

sumZero 5
sumZero 6
sumZero 25