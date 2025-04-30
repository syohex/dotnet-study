let findNumbers (nums: int list) : int =
    nums
    |> List.map string
    |> List.filter (fun s -> s.Length % 2 = 0)
    |> List.length

// 2
findNumbers [ 12; 345; 2; 6; 7896 ]

// 1
findNumbers [ 555; 901; 482; 1771 ]
