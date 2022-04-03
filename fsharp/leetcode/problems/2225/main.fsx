let findWinners (matches: (int * int) list) : int list list =
    let wins =
        matches
        |> List.fold (fun acc (win, _) -> Set.add win acc) Set.empty

    let loses =
        matches
        |> List.fold
            (fun acc (_, lose) ->
                match Map.tryFind lose acc with
                | None -> Map.add lose 1 acc
                | Some (v) -> Map.add lose (v + 1) acc)
            Map.empty

    let zeroLoses =
        wins
        |> Set.filter (fun n -> Map.containsKey n loses |> not)
        |> Set.toList
        |> List.sort

    let oneLoses =
        loses
        |> Map.filter (fun _ v -> v = 1)
        |> Map.keys
        |> Seq.toList
        |> List.sort

    [ zeroLoses; oneLoses ]

// [[1,2,10],[4,5,7,8]]
findWinners [ (1, 3)
              (2, 3)
              (3, 6)
              (5, 6)
              (5, 7)
              (4, 5)
              (4, 8)
              (4, 9)
              (10, 4)
              (10, 9) ]

// [[1,2,5,6],[]]
findWinners [ (2, 3)
              (1, 3)
              (5, 4)
              (6, 4) ]
