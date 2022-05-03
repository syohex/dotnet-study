open System

let findUnsortedSubarray (nums: int list) : int =
    let sorted = nums |> List.sort
    let len = nums.Length

    let vals =
        nums
        |> List.zip sorted
        |> List.mapi (fun i (a, b) -> i, a, b)

    let left, right =
        vals
        |> List.fold
            (fun (left, right) (i, a, b) ->
                if a <> b then
                    (Math.Min(left, i)), (Math.Max(right, i))
                else
                    left, right)
            (len, 0)

    if left = len then
        0
    else
        right - left + 1

// 5
findUnsortedSubarray [ 2
                       6
                       4
                       8
                       10
                       9
                       15 ]

// 0
findUnsortedSubarray [ 1; 2; 3; 4 ]

// 0
findUnsortedSubarray [ 0 ]
