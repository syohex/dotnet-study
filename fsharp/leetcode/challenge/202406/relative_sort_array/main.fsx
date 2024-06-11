let relativeSortArray (arr1: int list) (arr2: int list) : int list =
    let m =
        arr2 |> List.indexed |> List.fold (fun acc (i, n) -> Map.add n i acc) Map.empty

    arr1
    |> List.sortWith (fun a b ->
        match Map.tryFind a m, Map.tryFind b m with
        | None, None -> compare a b
        | Some(_), None -> -1
        | None, Some(_) -> 1
        | Some(i), Some(j) -> compare i j)

// [2,2,2,1,4,3,3,9,6,7,19]
relativeSortArray [ 2; 3; 1; 3; 2; 4; 6; 7; 9; 2; 19 ] [ 2; 1; 4; 3; 9; 6 ]

// [22,28,8,6,17,44]
relativeSortArray [ 28; 6; 22; 8; 44; 17 ] [ 22; 28; 8; 6 ]
