let search (nums: int[]) (target: int) : int =
    let rec findPivot (nums: int[]) left right =
        if left > right then
            left
        else
            let mid = left + (right - left) / 2

            if nums.[mid] > nums.[nums.Length - 1] then
                findPivot nums (mid + 1) right
            else
                findPivot nums left (mid - 1)

    let rec binarySearch (nums: int[]) left right target =
        if left > right then
            -1
        else
            let mid = left + (right - left) / 2

            if nums.[mid] = target then
                mid
            elif nums.[mid] < target then
                binarySearch nums (mid + 1) right target
            else
                binarySearch nums left (mid - 1) target

    let pivot = findPivot nums 0 (nums.Length - 1)
    let ret = binarySearch nums 0 (pivot - 1) target

    if ret <> -1 then
        ret
    else
        binarySearch nums pivot (nums.Length - 1) target

// 4
search [| 4; 5; 6; 7; 0; 1; 2 |] 0

// -1
search [| 4; 5; 6; 7; 0; 1; 2 |] 3

// -1
search [| 1 |] 0
