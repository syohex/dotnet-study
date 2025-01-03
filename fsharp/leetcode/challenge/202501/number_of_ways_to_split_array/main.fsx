let waysToSplitArray (nums: int list) : int =
    let rec waysToSplitArray' (nums: int64 list) left right acc =
        match nums with
        | [] -> failwith "never reach here"
        | _ :: [] -> acc
        | h :: t ->
            let left = left + h
            let right = right - h
            waysToSplitArray' t left right (if left >= right then acc + 1 else acc)

    let nums = nums |> List.map int64
    let right = List.sum nums
    waysToSplitArray' nums 0L right 0

// 2
waysToSplitArray [ 10; 4; -8; 7 ]

// 2
waysToSplitArray [ 2; 3; 1; 0 ]
