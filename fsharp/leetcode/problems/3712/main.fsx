let sumDivisibleByK (nums: int list) (k: int) : int =
    nums
    |> List.fold
        (fun acc n ->
            match Map.tryFind n acc with
            | Some v -> Map.add n (v + 1) acc
            | None -> Map.add n 1 acc)
        Map.empty
    |> Map.fold (fun acc n v -> if v % k = 0 then acc + n * v else acc) 0

// 16
sumDivisibleByK [ 1; 2; 2; 3; 3; 3; 3; 4 ] 2

// 0
sumDivisibleByK [ 1; 2; 3; 4; 5 ] 2

// 12
sumDivisibleByK [ 4; 4; 4; 1; 2; 3 ] 3
