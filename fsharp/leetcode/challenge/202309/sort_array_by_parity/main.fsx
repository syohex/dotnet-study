let sortArrayByParity (nums: int list) : int list =
    nums |> List.sortWith (fun a b -> compare (b % 2 = 0) (a % 2 = 0))

// [2;4;3;1]
sortArrayByParity [ 3; 1; 2; 4 ]

sortArrayByParity [ 0 ]
