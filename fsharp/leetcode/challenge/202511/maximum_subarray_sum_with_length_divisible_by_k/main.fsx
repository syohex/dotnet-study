let maxSubArraySum (nums: int list) (k: int) : int64 =
    let rec maxSubArraySum' nums sum (minSum: int64[]) acc =
        match nums with
        | [] -> acc
        | (i, h) :: t ->
            let m = i % k
            let sum = sum + int64 h
            let acc = max acc (sum - minSum.[m])
            minSum.[m] <- min minSum.[m] sum
            maxSubArraySum' t sum minSum acc

    let minSum = Array.init k (fun _ -> System.Int64.MaxValue / 2L)
    minSum.[k - 1] <- 0
    maxSubArraySum' (List.indexed nums) 0L minSum System.Int64.MinValue

// 3
maxSubArraySum [ 1; 2 ] 1

// -10
maxSubArraySum [ -1; -2; -3; -4; -5 ] 4

// 4
maxSubArraySum [ -5; 1; 2; -3; 4 ] 2
