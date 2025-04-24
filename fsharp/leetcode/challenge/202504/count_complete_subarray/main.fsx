let countCompleteSubArrays (nums: int[]) : int =
    let adjustLeftSide pos freq =
        if pos < 0 then
            freq
        else
            let v = Map.find nums.[pos] freq

            if v = 1 then
                Map.remove nums.[pos] freq
            else
                Map.add nums.[pos] (v - 1) freq

    let rec adjustRightSide right uniques freq =
        if right >= nums.Length || Map.count freq >= uniques then
            right, freq
        else
            let v = Map.tryFind nums.[right] freq |> Option.defaultValue 0
            adjustRightSide (right + 1) uniques (Map.add nums.[right] (v + 1) freq)

    let rec countCompleteSubArrays' left right freq uniques acc =
        if left >= nums.Length then
            acc
        else
            let freq = adjustLeftSide (left - 1) freq
            let right, freq = adjustRightSide right uniques freq

            if Map.count freq = uniques then
                countCompleteSubArrays' (left + 1) right freq uniques (acc + (nums.Length - right) + 1)
            else
                countCompleteSubArrays' (left + 1) right freq uniques acc

    let uniques = nums |> Set.ofArray |> Set.count
    countCompleteSubArrays' 0 0 Map.empty uniques 0

// 4
countCompleteSubArrays [| 1; 3; 1; 2; 2 |]

// 10
countCompleteSubArrays [| 5; 5; 5; 5 |]
