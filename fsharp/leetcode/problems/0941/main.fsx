let ascend (nums: int list) =
    let rec ascend' nums prev count =
        match nums with
        | [] -> -1
        | head :: tail ->
            if head = prev then -1
            else if head < prev then count
            else ascend' tail head (count + 1)

    ascend' (List.tail nums) (List.head nums) 1

let descend (nums: int list) =
    let rec descend' nums prev =
        match nums with
        | [] -> true
        | head :: tail ->
            if head >= prev then
                false
            else
                descend' tail head

    descend' (List.tail nums) (List.head nums)

let validMountainArray (nums: int list) : bool =
    let len = nums |> List.length

    if len < 3 then
        false
    else
        let pos = ascend nums

        if pos = -1 || pos = 1 then
            false
        else
            nums |> List.skip pos |> descend

// Tests

// false
validMountainArray [ 2; 1 ]

// false
validMountainArray [ 3; 5; 5 ]

// true
validMountainArray [ 0; 3; 2; 1 ]

// true
validMountainArray [ 0; 1; 2; 1 ]

// false
validMountainArray [ 1 .. 4 ]

// false
validMountainArray [ 4; 3; 2; 1 ]
