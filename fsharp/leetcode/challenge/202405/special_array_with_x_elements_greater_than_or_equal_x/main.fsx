let lowerBound (nums: int[]) (v: int) : int =
    let rec lowerBound' left right ret =
        if left > right then
            ret
        else
            let mid = left + (right - left) / 2

            if nums.[mid] >= v then
                lowerBound' left (mid - 1) mid
            else
                lowerBound' (mid + 1) right ret

    lowerBound' 0 (nums.Length - 1) nums.Length

let specialArray (nums: int list) : int =
    let nums = nums |> List.sort |> List.toArray

    seq { 1 .. nums.Length }
    |> Seq.tryFind (fun i ->
        let pos = lowerBound nums i
        i = nums.Length - pos)
    |> Option.defaultValue -1

// 2
specialArray [ 3; 5 ]

// -1
specialArray [ 0; 0 ]

// 3
specialArray [ 0; 4; 3; 0; 4 ]
