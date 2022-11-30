let uniqueOccurences (arr: int list) : bool =
    arr
    |> List.fold
        (fun acc n ->
            match Map.tryFind n acc with
            | Some (v) -> Map.add n (v + 1) acc
            | None -> Map.add n 1 acc)
        Map.empty
    |> Map.values
    |> Seq.fold
        (fun acc n ->
            match Map.tryFind n acc with
            | Some (v) -> Map.add n (v + 1) acc
            | None -> Map.add n 1 acc)
        Map.empty
    |> Map.forall (fun _ v -> v = 1)

// true
uniqueOccurences [ 1; 2; 2; 1; 1; 3 ]

// false
uniqueOccurences [ 1; 2 ]

// true
uniqueOccurences [ -3
                   0
                   1
                   -3
                   1
                   1
                   1
                   -3
                   10
                   0 ]
