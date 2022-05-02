let sortArrayByParity (nums: int list) : int list =
    nums
    |> List.sortWith (fun a b ->
        match a &&& 1, b &&& 1 with
        | 0, 0 -> compare a b
        | 0, 1 -> -1
        | 1, 0 -> 1
        | _, _ -> compare a b)


// [2;4;1;3]
sortArrayByParity [ 3; 1; 2; 4 ]

// [0]
sortArrayByParity [ 0 ]
