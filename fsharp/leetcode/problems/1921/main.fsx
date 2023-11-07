let eliminateMaximum (dist: int list) (speed: int list) : int =
    List.zip dist speed
    |> List.map (fun (d, s) -> double d / double s)
    |> List.sort
    |> List.indexed
    |> List.fold (fun acc (i, time) -> if time <= i then acc else acc + 1) 0

// 3
eliminateMaximum [ 1; 3; 4 ] [ 1; 1; 1 ]

// 1
eliminateMaximum [ 1; 1; 2; 3 ] [ 1; 1; 1; 1 ]

// 1
eliminateMaximum [ 3; 2; 4 ] [ 5; 3; 2 ]

// 2
eliminateMaximum [ 4; 2 ] [ 5; 1 ]
