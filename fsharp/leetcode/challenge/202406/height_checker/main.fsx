let heightChecker (heights: int list) : int =
    heights
    |> List.zip (List.sort heights)
    |> List.filter (fun (a, b) -> a <> b)
    |> List.length

// 3
heightChecker [ 1; 1; 4; 2; 1; 3 ]

// 5
heightChecker [ 5; 1; 2; 3; 4 ]

// 0
heightChecker [ 1; 2; 3; 4; 5 ]
