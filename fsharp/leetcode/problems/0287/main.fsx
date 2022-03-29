let findDuplicate (nums: int list) : int =
    let rec findDuplicate' (nums: int []) left right =
        if left > right then
            left
        else
            let mid = left + ((right - left) / 2)

            let count =
                nums
                |> Array.fold (fun acc n -> if n <= mid then acc + 1 else acc) 0

            if count <= mid then
                findDuplicate' nums (mid + 1) right
            else
                findDuplicate' nums left (mid - 1)

    findDuplicate' (nums |> Array.ofList) 1 (nums.Length - 1)

// 2
findDuplicate [ 1; 3; 4; 2; 2 ]

// 3
findDuplicate [ 3; 1; 3; 4; 2 ]

// 2
findDuplicate [ 2; 2; 2; 2; 2 ]