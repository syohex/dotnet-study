let countSubArrays (nums: int list) (k: int64) : int64 =
    let rec adjustLeft (nums: int64[]) left right (sum: int64) =
        if left > right then
            left, sum
        else
            let v = sum * int64 (right - left + 1)

            if v >= k then
                adjustLeft nums (left + 1) right (sum - nums.[left])
            else
                left, sum

    let rec countSubArrays' (nums: int64[]) left right sum acc =
        if right >= nums.Length then
            acc
        else
            let sum = sum + nums.[right]
            let left, sum = adjustLeft nums left right sum

            if left <= right then
                countSubArrays' nums left (right + 1) sum (acc + right - left + 1)
            else
                countSubArrays' nums left (right + 1) sum acc

    let nums = nums |> List.toArray |> Array.map int64
    countSubArrays' nums 0 0 0 0

// 6
countSubArrays [ 2; 1; 4; 3; 5 ] 10

// 5
countSubArrays [ 1; 1; 1 ] 5

// 3
countSubArrays [ 9; 5; 3; 8; 4; 7; 2; 7; 4; 5; 4; 9; 1; 4; 8; 10; 8; 10; 4; 7 ] 4
