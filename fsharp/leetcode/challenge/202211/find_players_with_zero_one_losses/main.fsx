let findWinners (matches: (int * int) list) : int list * int list =
    let losers =
        matches
        |> List.fold
            (fun acc (_, loser) ->
                match Map.tryFind loser acc with
                | None -> Map.add loser 1 acc
                | Some (v) -> Map.add loser (v + 1) acc)
            Map.empty

    let winners =
        matches
        |> List.fold
            (fun acc (winner, _) ->
                match Map.tryFind winner acc with
                | None -> Map.add winner 1 acc
                | Some (v) -> Map.add winner (v + 1) acc)
            Map.empty
        |> Map.filter (fun k _ -> Map.containsKey k losers |> not)
        |> Map.keys
        |> Seq.toList
        |> List.sort

    let losers' =
        losers
        |> Map.filter (fun _ v -> v = 1)
        |> Map.keys
        |> Seq.toList
        |> List.sort

    winners, losers'

// ([1, 2, 10], [4, 5, 7, 8])
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

// ([1, 2, 5, 6], [])
findWinners [ (2, 3)
              (1, 3)
              (5, 4)
              (6, 4) ]
