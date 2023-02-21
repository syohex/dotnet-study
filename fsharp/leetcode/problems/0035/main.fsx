let searchInsert (nums: int[]) (target: int) : int =
    let rec searchInsert' (nums: int[]) left right =
        if left > right then
            left
        else
            let mid = left + (right - left) / 2

            if nums.[mid] = target then
                mid
            elif target < nums.[mid] then
                searchInsert' nums left (mid - 1)
            else
                searchInsert' nums (mid + 1) right

    searchInsert' nums 0 (nums.Length - 1)

// 2
searchInsert [| 1; 3; 5; 6 |] 5

// 1
searchInsert [| 1; 3; 5; 6 |] 2

// 4
searchInsert [| 1; 3; 5; 6 |] 7

// 0
searchInsert [| 1 |] 1
