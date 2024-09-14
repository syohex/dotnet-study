let longestSubArray (nums: int list) : int =
    let rec longestSubArray' nums maxVal len acc =
        match nums with
        | [] -> acc
        | h :: t ->
            let maxVal, len, acc = if h > maxVal then h, 0, 0 else maxVal, len, acc

            if h = maxVal then
                let len = len + 1
                longestSubArray' t maxVal len (max acc len)
            else
                longestSubArray' t maxVal 0 acc

    longestSubArray' nums 0 0 0

// 2
longestSubArray [ 1; 2; 3; 3; 2; 2 ]

// 1
longestSubArray [ 1; 2; 3; 4 ]
