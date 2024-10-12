let minGroups (intervals: (int * int) list) : int =
    intervals
    |> List.fold (fun acc (s, e) -> (e + 1, -1) :: (s, 1) :: acc) []
    |> List.sort
    |> List.fold
        (fun (ret, duplicates) (_, n) ->
            let duplicates' = duplicates + n
            max ret duplicates', duplicates')
        (0, 0)
    |> fst

// 3
minGroups [ (5, 10); (6, 8); (1, 5); (2, 3); (1, 10) ]

// 1
minGroups [ (1, 3); (5, 6); (8, 10); (11, 13) ]
