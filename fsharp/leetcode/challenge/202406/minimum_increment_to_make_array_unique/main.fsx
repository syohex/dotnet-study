let minIncrementForUnique (nums: int list) : int =
    let rec minIncrementForUnique' i (nums: int[]) acc =
        if i >= nums.Length then
            acc
        else if nums.[i] <= nums.[i - 1] then
            let increment = nums.[i - 1] - nums.[i] + 1
            nums.[i] <- nums.[i - 1] + 1
            minIncrementForUnique' (i + 1) nums (acc + increment)
        else
            minIncrementForUnique' (i + 1) nums acc

    let nums = nums |> List.sort |> List.toArray
    minIncrementForUnique' 1 nums 0

// 1
minIncrementForUnique [ 1; 2; 2 ]

// 6
minIncrementForUnique [ 3; 2; 1; 2; 1; 7 ]
