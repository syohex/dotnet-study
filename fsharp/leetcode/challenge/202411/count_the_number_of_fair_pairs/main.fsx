let countFairPair (nums: int list) (lower: int) (upper: int) : int64 =
    let rec lowerBound left right v (nums: int[]) =
        if left > right then
            left
        else
            let mid = left + (right - left) / 2

            if nums.[mid] < v then
                lowerBound (mid + 1) right v nums
            else
                lowerBound left (mid - 1) v nums

    let nums = nums |> List.sort |> List.toArray
    let limit = nums.Length - 1

    nums
    |> Array.indexed
    |> Array.fold
        (fun acc (i, n) ->
            let left = lowerBound (i + 1) limit (lower - n) nums
            let right = lowerBound (i + 1) limit (upper - n + 1) nums
            acc + int64 (right - left))
        0L

// 6
countFairPair [ 0; 1; 7; 4; 4; 5 ] 3 6

// 1
countFairPair [ 1; 7; 9; 2; 5 ] 11 11
