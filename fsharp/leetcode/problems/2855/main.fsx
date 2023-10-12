let minimumRightShiftsToSortTheArray (nums: int list) : int =
    let rec minIndex nums min idx =
        match nums with
        | [] -> idx
        | (i, h) :: t -> if h < min then minIndex t h i else minIndex t min idx

    let sorted = List.sort nums
    let len = nums.Length

    let minIdx =
        minIndex (List.indexed nums) System.Int32.MaxValue System.Int32.MaxValue

    let ns = (List.skip minIdx nums) @ (List.take minIdx nums)
    if ns = sorted then (len - minIdx) % len else -1

// 2
minimumRightShiftsToSortTheArray [ 3; 4; 5; 1; 2 ]

// 0
minimumRightShiftsToSortTheArray [ 1; 3; 5 ]

// -1
minimumRightShiftsToSortTheArray [ 2; 1; 4 ]
