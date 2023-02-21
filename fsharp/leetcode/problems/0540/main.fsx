let singleNonDuplicate (nums: int[]) : int =
    let rec singleNonDuplicate' (nums: int[]) left right =
        if left >= right then
            nums.[left]
        else
            let mid = left + (right - left) / 2
            let mid = if mid % 2 = 1 then mid - 1 else mid

            if nums.[mid] = nums.[mid + 1] then
                singleNonDuplicate' nums (mid + 2) right
            else
                singleNonDuplicate' nums left mid

    singleNonDuplicate' nums 0 (nums.Length - 1)

// 2
singleNonDuplicate [| 1; 1; 2; 3; 3; 4; 4; 8; 8 |]

// 10
singleNonDuplicate [| 3; 3; 7; 7; 10; 11; 11 |]
