let frequencySort (nums: int list) : int list =
    let freq = nums |> List.countBy id |> Map.ofList

    nums
    |> List.sortWith (fun a b ->
        match Map.find a freq, Map.find b freq with
        | x, y when x = y -> compare b a
        | x, y -> compare x y)

// [3,1,1,2,2,2]
frequencySort [ 1; 1; 2; 2; 2; 3 ]

// [1,3,3,2,2]
frequencySort [ 2; 3; 1; 3; 2 ]

// [5,-1,4,4,-6,-6,1,1,1]
frequencySort [ -1; 1; -6; 4; 5; -6; 1; 4; 1 ]
