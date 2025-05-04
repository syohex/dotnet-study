let numEquivDominoPairs (dominoes: (int * int) list) : int =
    dominoes
    |> List.fold
        (fun acc (a, b) ->
            let k = if a < b then a, b else b, a
            let v = Map.tryFind k acc |> Option.defaultValue 0
            Map.add k (v + 1) acc)
        Map.empty
    |> Map.values
    |> Seq.filter (fun v -> v >= 2)
    |> Seq.fold (fun acc v -> acc + ((v * (v - 1)) / 2)) 0

// 1
numEquivDominoPairs [ (1, 2); (2, 1); (3, 4); (5, 6) ]

// 3
numEquivDominoPairs [ (1, 2); (1, 2); (1, 1); (1, 2); (2, 2) ]
