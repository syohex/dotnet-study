let hasTrailingZeros (nums: int list) : bool =
    nums |> List.filter (fun n -> n % 2 = 0) |> List.length |> (fun n -> n >= 2)

// true
hasTrailingZeros [ 1; 2; 3; 4; 5 ]

// true
hasTrailingZeros [ 2; 4; 8; 16 ]

// false
hasTrailingZeros [ 1; 3; 5; 7; 9 ]
