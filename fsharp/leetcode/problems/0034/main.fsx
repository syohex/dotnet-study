let searchRange (nums: int[]) (target: int) : (int * int) =
    let rec searchRange' (nums: int[]) left right target leftBound =
        if left > right then
            None
        else
            let mid = left + (right - left) / 2

            if nums.[mid] = target then
                if leftBound then
                    if mid = left || nums.[mid - 1] <> target then
                        Some(mid)
                    else
                        searchRange' nums left (mid - 1) target leftBound
                else if mid = right || nums.[mid + 1] <> target then
                    Some(mid)
                else
                    searchRange' nums (mid + 1) right target leftBound
            elif nums.[mid] < target then
                searchRange' nums (mid + 1) right target leftBound
            else
                searchRange' nums left (mid - 1) target leftBound

    match searchRange' nums 0 (nums.Length - 1) target true with
    | None -> (-1, -1)
    | Some(leftBound) ->
        match searchRange' nums 0 (nums.Length - 1) target false with
        | None -> failwith "never reach here"
        | Some(rightBound) -> (leftBound, rightBound)

// (3, 4)
searchRange [| 5; 7; 7; 8; 8; 10 |] 8

// (-1, -1)
searchRange [| 5; 7; 7; 8; 8; 10 |] 6

// (-1, -1)
searchRange [||] 0
