let longestSubArray (nums: int list) : int =
    let rec longestSubArray' nums maxVal acc ret =
        match nums with
        | [] -> max acc ret
        | h :: t ->
            if h = maxVal then
                longestSubArray' t maxVal (acc + 1) ret
            else
                longestSubArray' t maxVal acc (max ret acc)

    let maxVal = List.max nums
    longestSubArray' nums maxVal 0 0

// 2
longestSubArray [ 1; 2; 3; 3; 2; 2 ]

// 1
longestSubArray [ 1; 2; 3; 4 ]
