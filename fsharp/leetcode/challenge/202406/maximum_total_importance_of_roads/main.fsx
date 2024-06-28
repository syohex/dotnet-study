let maximumImportance (n: int) (roads: (int * int) list) : int64 =
    let freq =
        roads
        |> List.fold
            (fun (acc: int[]) (a, b) ->
                acc.[a] <- acc.[a] + 1
                acc.[b] <- acc.[b] + 1
                acc)
            (Array.zeroCreate n)
        |> Array.map int64

    freq
    |> Array.sort
    |> Array.indexed
    |> Array.fold (fun acc (i, v) -> acc + (int64 (i + 1)) * v) 0

// 43
maximumImportance 5 [ (0, 1); (1, 2); (2, 3); (0, 2); (1, 3); (2, 4) ]

// 20
maximumImportance 5 [ (0, 3); (2, 4); (1, 3) ]
