let countOdds (low: int) (high: int) : int =
    match low % 2 = 1, high % 2 = 1 with
    | true, true -> (high - low + 1) / 2 + 1
    | _ -> (high - low + 1) / 2

// 3
countOdds 3 7

// 1
countOdds 8 10

// 0
countOdds 4 4

// 1
countOdds 3 3

// 2
countOdds 3 6

// 3
countOdds 4 9
