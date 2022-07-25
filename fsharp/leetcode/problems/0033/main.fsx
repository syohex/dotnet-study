let searchRange (nums: int []) (target: int) : (int * int) =
    let rec checkUnder pos (nums: int []) target =
        if pos < 0 then 0
        elif nums.[pos] <> target then pos + 1
        else checkUnder (pos - 1) nums target

    let rec checkUpper pos (nums: int []) target =
        if pos >= nums.Length then
            nums.Length - 1
        elif nums.[pos] <> target then
            pos - 1
        else
            checkUpper (pos + 1) nums target

    let rec searchRange' left right (nums: int []) target =
        if left > right then
            -1, -1
        else
            let mid = (left + right) / 2

            if nums.[mid] = target then
                checkUnder (mid - 1) nums target, checkUpper (mid + 1) nums target
            elif nums.[mid] < target then
                searchRange' (mid + 1) right nums target
            else
                searchRange' left (mid - 1) nums target

    searchRange' 0 (nums.Length - 1) nums target

// (3, 4)
searchRange [| 5; 7; 7; 8; 8; 10 |] 8

// (-1, -1)
searchRange [| 5; 7; 7; 8; 8; 10 |] 6

// (-1, -1)
searchRange [||] 0

// (0, 4)
searchRange [| 1; 1; 1; 1; 1 |] 1

// (0, 3)
searchRange [| 1; 1; 1; 1; 2 |] 1

// (1, 4)
searchRange [| 0; 1; 1; 1; 1 |] 1
