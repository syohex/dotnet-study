let search (nums: int []) (target: int) : int =
    let rec search' (nums: int []) left right target =
        if left > right then
            -1
        else
            let mid = left + (right - left) / 2

            if nums.[mid] = target then
                mid
            elif nums.[mid] < target then
                search' nums (mid + 1) right target
            else
                search' nums left (mid - 1) target

    search' nums 0 (nums.Length - 1) target

// 4
search [| -1; 0; 3; 5; 9; 12 |] 9

// -1
search [| -1; 0; 3; 5; 9; 12 |] 2

// 0
search [| 5 |] 5

// -1
search [| -5 |] 5
