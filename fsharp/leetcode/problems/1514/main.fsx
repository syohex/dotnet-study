let maxProhability (n: int) (edges: (int * int) list) (succProb: double list) (start: int) (ends: int) : double =
    let rec toGraph edges succProb graph scores =
        match edges, succProb with
        | [], [] -> graph, scores
        | (p1, p2) :: t1, score :: t2 ->
            let graph' =
                match Map.tryFind p1 graph with
                | Some(v) -> Map.add p1 (p2 :: v) graph
                | None -> Map.add p1 [ p2 ] graph

            let graph'' =
                match Map.tryFind p2 graph' with
                | Some(v) -> Map.add p2 (p1 :: v) graph'
                | None -> Map.add p2 [ p1 ] graph'

            let scores' = Map.add (p1, p2) score scores
            let scores'' = Map.add (p2, p1) score scores'
            toGraph t1 t2 graph'' scores''
        | _ -> failwith "never reach here"

    let rec maxProhability' q graph scores (maxs: double[]) =
        match q with
        | [] -> maxs.[ends]
        | node :: t ->
            match Map.tryFind node graph with
            | None -> maxProhability' t graph scores maxs
            | Some(nexts) ->
                let q' =
                    nexts
                    |> List.fold
                        (fun acc nextNode ->
                            match Map.tryFind (node, nextNode) scores with
                            | None -> acc
                            | Some(score) ->
                                let newScore = score * maxs.[node]

                                if newScore > maxs.[nextNode] then
                                    maxs.[nextNode] <- newScore
                                    nextNode :: acc
                                else
                                    acc)
                        t

                maxProhability' q' graph scores maxs

    let graph, scores = toGraph edges succProb Map.empty Map.empty
    let maxs: double[] = Array.zeroCreate n
    maxs.[start] <- 1.0

    maxProhability' [ start ] graph scores maxs

// 0.25
maxProhability 3 [ (0, 1); (1, 2); (0, 2) ] [ 0.5; 0.5; 0.2 ] 0 2

// 0.3
maxProhability 3 [ (0, 1); (1, 2); (0, 2) ] [ 0.5; 0.5; 0.3 ] 0 2

// 0
maxProhability 3 [ (0, 1) ] [ 0.5 ] 0 2
