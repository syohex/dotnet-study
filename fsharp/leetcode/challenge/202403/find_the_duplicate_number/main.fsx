let findDuplicate (nums: int list) : int =
    let rec findDuplicate' nums left right ret =
        if left > right then
            ret
        else
            let mid = left + (right - left) / 2
            let count = List.filter (fun num -> num <= mid) nums |> List.length

            if count > mid then
                findDuplicate' nums left (mid - 1) mid
            else
                findDuplicate' nums (mid + 1) right ret

    findDuplicate' nums 1 (1 + List.length nums) 0

// 2
findDuplicate [ 1; 3; 4; 2; 2 ]

// 3
findDuplicate [ 3; 1; 3; 4; 2 ]

// 3
findDuplicate [ 3; 3; 3; 3; 3 ]
