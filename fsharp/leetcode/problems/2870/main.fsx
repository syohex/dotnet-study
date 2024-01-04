let minOperations (nums: int list) : int =
    let freq =
        nums
        |> List.fold (fun acc n -> Map.add n ((Map.tryFind n acc |> Option.defaultValue 0) + 1) acc) Map.empty

    if Map.values freq |> Seq.tryFind ((=) 1) |> Option.isSome then
        -1
    else
        Map.values freq
        |> Seq.fold (fun acc n -> if n % 3 = 0 then acc + (n / 3) else acc + (n / 3) + 1) 0

// 4
minOperations [ 2; 3; 3; 2; 2; 4; 2; 3; 4 ]

// -1
minOperations [ 2; 2; 1; 3; 3; 3; 4; 4 ]
