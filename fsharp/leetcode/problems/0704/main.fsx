let search (nums: int list) (target: int) : int =
    let rec search' (nums: int []) left right =
        if left > right then
            -1
        else
            let mid = left + ((right - left) / 2)
            let v = nums.[mid]

            if target = v then
                mid
            elif target < v then
                search' nums left (mid - 1)
            else
                search' nums (mid + 1) right

    let nums' = nums |> List.toArray
    search' nums' 0 (nums'.Length - 1)

// 4
search [ -1; 0; 3; 5; 9; 12 ] 9

// -1
search [ -1; 0; 3; 5; 9; 12 ] 2

// 0
search [ 5 ] 5
