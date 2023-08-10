let search (nums: int[]) (target: int) : bool =
    let rec search' (nums: int[]) left right target =
        if left > right then
            false
        else
            let mid = left + (right - left) / 2

            if nums.[mid] = target then
                true
            elif nums.[left] < nums.[mid] then
                if nums.[mid] > target && nums.[left] <= target then
                    search' nums left (mid - 1) target
                else
                    search' nums (mid + 1) right target
            elif nums.[left] > nums.[mid] then
                if nums.[mid] < target && target < nums.[left] then
                    search' nums (mid + 1) right target
                else
                    search' nums left (mid - 1) target
            else
                search' nums (left + 1) right target

    search' nums 0 (nums.Length - 1) target

// true
search [| 2; 5; 6; 0; 0; 1; 2 |]  0

// false
search [| 2; 5; 6; 0; 0; 1; 2 |] 3

// true
search [| 1; 1; 1; 1; 0; 1; 1; 1 |] 0
