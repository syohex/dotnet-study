let divideArray (nums: int list) : bool =
    nums
    |> List.fold
        (fun acc n ->
            let v = Map.tryFind n acc |> Option.defaultValue 0
            Map.add n (v + 1) acc)
        Map.empty
    |> Map.values
    |> Seq.forall (fun v -> v % 2 = 0)

// true
divideArray [ 3; 2; 3; 2; 2; 2 ]

// false
divideArray [ 1; 2; 3; 4 ]
