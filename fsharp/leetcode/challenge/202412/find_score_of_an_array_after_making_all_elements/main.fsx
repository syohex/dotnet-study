let findScore (nums: int list) : int64 =
    nums
    |> List.indexed
    |> List.map (fun (i, n) -> int64 n, i)
    |> List.sort
    |> List.fold
        (fun (acc, visited) (n, i) ->
            if Set.contains i visited then
                acc, visited
            else
                let visited = visited |> Set.add i |> Set.add (i - 1) |> Set.add (i + 1)
                acc + n, visited)
        (0L, Set.empty)
    |> fst

// 7
findScore [ 2; 1; 3; 4; 5; 2 ]

// 5
findScore [ 2; 3; 5; 1; 3; 2 ]
