let maxSubArray (nums: int list) : int =
    let rec maxSubArray' (nums: int list) (sum: int) (max: int) : int =
        match nums with
        | [] -> max
        | n :: tail ->
            if sum + n < n then
                maxSubArray' tail n (if n > max then n else max)
            else
                maxSubArray' tail (sum + n) (if sum + n > max then sum + n else max)

    let first = List.head nums
    maxSubArray' (List.tail nums) first first

maxSubArray [ -2
              1
              -3
              4
              -1
              2
              1
              -5
              4 ]

maxSubArray [ 1 ]
maxSubArray [ 5; 4; -1; 7; 8 ]
