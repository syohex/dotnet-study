open System

let minSubArray (target: int) (nums: int[]) : int =
    let rec shrinkWindow left right sum target (nums: int[]) (ret: int) =
        if sum < target then
            left, sum, ret
        else
            let ret' = Math.Min(ret, right - left + 1)
            let sum' = sum - nums.[left]
            shrinkWindow (left + 1) right sum' target nums ret'

    let rec minSubArray' left right sum target (nums: int[]) ret =
        if right = nums.Length then
            if ret = Int32.MaxValue then 0 else ret
        else
            let left', sum', ret' = shrinkWindow left right (sum + nums.[right]) target nums ret
            minSubArray' left' (right + 1) sum' target nums ret'

    minSubArray' 0 0 0 target nums (Int32.MaxValue)

// 2
minSubArray 7 [| 2; 3; 1; 2; 4; 3 |]

// 1
minSubArray 4 [| 1; 4; 4 |]

// 0
minSubArray 11 [| 1; 1; 1; 1; 1; 1; 1; 1 |]
