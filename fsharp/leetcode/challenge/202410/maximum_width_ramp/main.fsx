let maxWidthRamp (nums: int list) : int =
    let rec maxWidthRamp' left right (nums: int[]) (rightLargests: int[]) acc =
        if right >= nums.Length then
            acc
        elif left < right && nums.[left] > rightLargests.[right] then
            maxWidthRamp' (left + 1) right nums rightLargests acc
        else
            let acc' = max acc (right - left)
            maxWidthRamp' left (right + 1) nums rightLargests acc'

    let rightLargests =
        nums
        |> List.rev
        |> List.fold (fun (acc, prev) num -> if num > prev then num :: acc, num else prev :: acc, prev) ([], -1)
        |> fst
        |> List.rev
        |> List.toArray

    maxWidthRamp' 0 0 (List.toArray nums) rightLargests 0

// 4
maxWidthRamp [ 6; 0; 8; 2; 1; 5 ]

// 7
maxWidthRamp [ 9; 8; 1; 0; 1; 9; 4; 0; 4; 1 ]
