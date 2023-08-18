open System

let maximalNetworkRank (n: int) (roads: (int * int) list) : int =
    let toGraph n roads =
        let rec toGraph' roads acc =
            match roads with
            | [] -> acc
            | (p1, p2) :: t ->
                let acc' =
                    acc
                    |> Map.add p1 (Set.add p2 (Map.find p1 acc))
                    |> Map.add p2 (Set.add p1 (Map.find p2 acc))

                toGraph' t acc'

        let acc =
            seq { 0 .. (n - 1) }
            |> Seq.fold (fun acc i -> Map.add i Set.empty acc) Map.empty

        toGraph' roads acc

    let rec maximalNetworkRank' i n graph acc =
        if i >= n - 1 then
            acc
        else
            let edges1 = Map.find i graph |> Set.count

            let acc' =
                seq { (i + 1) .. (n - 1) }
                |> Seq.fold
                    (fun acc j ->
                        let s = Map.find j graph
                        let edges2 = Set.count s

                        if Set.contains i s then
                            Math.Max(acc, edges1 + edges2 - 1)
                        else
                            Math.Max(acc, edges1 + edges2))
                    acc

            maximalNetworkRank' (i + 1) n graph acc'

    let graph = toGraph n roads
    maximalNetworkRank' 0 n graph 0

// 4
maximalNetworkRank 4 [ (0, 1); (0, 3); (1, 2); (1, 3) ]

// 5
maximalNetworkRank 5 [ (0, 1); (0, 3); (1, 2); (1, 3); (2, 3); (2, 4) ]

// 5
maximalNetworkRank 8 [ (0, 1); (1, 2); (2, 3); (2, 4); (5, 6); (5, 7) ]
