let numberOfPairs (nums: int list) : (int * int) =
    let freq =
        nums
        |> List.fold
            (fun acc n ->
                match Map.tryFind n acc with
                | Some (v) -> Map.add n (v + 1) acc
                | None -> Map.add n 1 acc)
            Map.empty

    freq
    |> Map.fold (fun (pairs, rest) _ v -> pairs + (v / 2), rest + (v % 2)) (0, 0)

// [3, 1]
numberOfPairs [ 1; 3; 2; 1; 3; 2; 2 ]

// [1, 0]
numberOfPairs [ 1; 1 ]

// [0, 1]
numberOfPairs [ 0 ]
