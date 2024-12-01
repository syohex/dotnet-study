let checkIfExist (arr: int list) : bool =
    let h =
        arr
        |> List.indexed
        |> List.fold
            (fun acc (i, n) ->
                match Map.tryFind n acc with
                | Some(v) -> Map.add n (i :: v) acc
                | None -> Map.add n [ i ] acc)
            Map.empty

    arr
    |> List.indexed
    |> List.exists (fun (i, n) ->
        match Map.tryFind (2 * n) h with
        | None -> false
        | Some(v) -> List.exists (fun j -> i <> j) v)

// true
checkIfExist [ 10; 2; 5; 3 ]

// false
checkIfExist [ 3; 1; 7; 11 ]
