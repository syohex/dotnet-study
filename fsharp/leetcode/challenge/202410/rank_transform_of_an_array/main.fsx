let arrayRankTransform (arr: int list) : int list =
    let m =
        arr
        |> List.countBy id
        |> List.map fst
        |> List.sort
        |> List.indexed
        |> List.fold (fun acc (i, n) -> Map.add n (i + 1) acc) Map.empty

    arr |> List.map (fun n -> Map.find n m)

// [4,1,2,3]
arrayRankTransform [ 40; 10; 20; 30 ]

// [1,1,1]
arrayRankTransform [ 100; 100; 100 ]

// [5,3,4,2,8,6,7,1,3]
arrayRankTransform [ 37; 12; 28; 9; 100; 56; 80; 5; 12 ]
