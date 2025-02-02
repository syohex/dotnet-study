let check (nums: int list) : bool =
    let last = List.last nums

    let count, _ =
        nums
        |> List.fold (fun (acc, prev) n -> if prev > n then acc + 1, n else acc, 0) (0, last)

    count <= 1

// true
check [ 3; 4; 5; 1; 2 ]

// false
check [ 2; 1; 3; 4 ]

// true
check [ 1; 2; 3 ]

// true
check [ 6; 10; 6 ]

// true
check [ 10; 1; 1; 1; 2; 3 ]
