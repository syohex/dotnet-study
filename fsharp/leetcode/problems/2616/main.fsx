let minimizeMax (nums: int list) (p: int) : int =
    let countValids (nums: int[]) (threshold: int) : int =
        let rec countValids' i (nums: int[]) threshold acc =
            if i >= nums.Length then
                acc
            else if nums.[i] - nums.[i - 1] <= threshold then
                countValids' (i + 2) nums threshold (acc + 1)
            else
                countValids' (i + 1) nums threshold acc

        countValids' 1 nums threshold 0

    let rec minimizeMax' (nums: int[]) left right p =
        if left >= right then
            left
        else
            let mid = left + (right - left) / 2

            if countValids nums mid >= p then
                minimizeMax' nums left mid p
            else
                minimizeMax' nums (mid + 1) right p

    let nums' = nums |> List.sort |> List.toArray
    let right = nums'.[nums.Length - 1] - nums'.[0]
    minimizeMax' nums' 0 right p

// 1
minimizeMax [ 10; 1; 2; 7; 1; 3 ] 2

// 0
minimizeMax [ 4; 2; 1; 2 ] 1
