let getSneakyNumbers (nums: int list) : int list =
    nums |> List.countBy id |> List.filter (snd >> (=) 2) |> List.map fst

// [0, 1]
getSneakyNumbers [ 0; 1; 1; 0 ]

// [2;3]
getSneakyNumbers [ 0; 3; 2; 1; 3; 2 ]

// [4,5]
getSneakyNumbers [ 7; 1; 5; 4; 3; 4; 6; 0; 9; 5; 8; 2 ]

// [0, 2]
getSneakyNumbers [ 2; 0; 2; 1; 0 ]
