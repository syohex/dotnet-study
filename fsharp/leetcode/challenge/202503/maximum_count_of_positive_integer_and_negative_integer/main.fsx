let lowerBound (nums: int[]) (v: int) : int =
    let rec lowerBound' left right =
        if left > right then
            left
        else
            let mid = left + (right - left) / 2

            if nums.[mid] < v then
                lowerBound' (mid + 1) right
            else
                lowerBound' left (mid - 1)

    lowerBound' 0 (nums.Length - 1)

let maximumCount (nums: int[]) : int =
    let zeroBound = lowerBound nums 0
    let oneBound = lowerBound nums 1
    max zeroBound (nums.Length - oneBound)

// 3
maximumCount [| -2; -1; -1; 1; 2; 3 |]

// 3
maximumCount [| -3; -2; -1; 0; 0; 1; 2 |]

// 4
maximumCount [| 5; 20; 66; 1314 |]
