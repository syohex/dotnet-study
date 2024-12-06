let maxCount (banned: int list) (n: int) (maxSum: int) : int =
    let rec maxCount' i n count sum banned =
        if i > n then
            count
        else if Set.contains i banned then
            maxCount' (i + 1) n count sum banned
        else if i > sum then
            count
        else
            maxCount' (i + 1) n (count + 1) (sum - i) banned

    maxCount' 1 n 0 maxSum (Set.ofList banned)

// 2
maxCount [ 1; 6; 5 ] 5 6

// 0
maxCount [ 1..7 ] 8 1

// 7
maxCount [ 11 ] 7 50
