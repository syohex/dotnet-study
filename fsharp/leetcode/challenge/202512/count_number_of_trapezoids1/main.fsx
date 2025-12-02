let countTrapezoids (points: (int * int) list) : int =
    let modulo = 1_000_000_007L

    let rec countTrapezoids' ys totalEdges acc =
        match ys with
        | [] -> acc % modulo |> int
        | h :: t ->
            let edges = (h * (h - 1)) / 2
            let acc = acc + int64 edges * totalEdges
            let totalEdges = totalEdges + int64 edges
            countTrapezoids' t totalEdges acc

    let ys =
        points
        |> List.fold
            (fun acc (_, y) ->
                match Map.tryFind y acc with
                | Some v -> Map.add y (v + 1) acc
                | None -> Map.add y 1 acc)
            Map.empty
        |> Map.values
        |> Seq.toList

    countTrapezoids' ys 0L 0L

// 3
countTrapezoids [ (1, 0); (2, 0); (3, 0); (2, 2); (3, 2) ]

// 1
countTrapezoids [ (0, 0); (1, 0); (0, 1); (2, 1) ]
