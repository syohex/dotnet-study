let finalValueAfterOperations (operations: string list) : int =
    operations
    |> List.fold (fun acc s -> if s.[1] = '+' then acc + 1 else acc - 1) 0

// 1
finalValueAfterOperations [ "--X"; "X++"; "X++" ]

// 3
finalValueAfterOperations [ "++X"; "++X"; "X++" ]

// 0
finalValueAfterOperations [ "X++"; "++X"; "--X"; "X--" ]
