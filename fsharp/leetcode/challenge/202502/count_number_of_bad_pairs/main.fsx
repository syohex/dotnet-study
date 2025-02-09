let countBadPairs (nums: int list) : int64 =
    let h =
        nums
        |> List.indexed
        |> List.fold
            (fun acc (i, n) ->
                let diff = n - i
                let v = Map.tryFind diff acc |> Option.defaultValue 0L
                Map.add diff (v + 1L) acc)
            Map.empty

    let len = List.length nums

    nums
    |> List.indexed
    |> List.fold
        (fun (acc, (rest, h)) (i, n) ->
            let diff = n - i
            let count = Map.find diff h
            acc + rest - count, (rest - 1L, Map.add diff (count - 1L) h))
        (0L, (int64 len, h))
    |> fst

// 5
countBadPairs [ 4; 1; 3; 3 ]

// 0
countBadPairs [ 1; 2; 3; 4; 5 ]
