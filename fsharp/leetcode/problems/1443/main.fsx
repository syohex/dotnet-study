let edgesToGraph (edges: (int * int) list) =
    let rec edgesToGraph' edges acc =
        match edges with
        | [] -> acc
        | (s, e) :: t ->
            let acc' =
                match Map.tryFind s acc with
                | None -> Map.add s [ e ] acc
                | Some(v) -> Map.add s (e :: v) acc

            let acc'' =
                match Map.tryFind e acc with
                | None -> Map.add e [ s ] acc'
                | Some(v) -> Map.add e (s :: v) acc'

            edgesToGraph' t acc''

    edgesToGraph' edges Map.empty

let minTime (n: int) (edges: (int * int) list) (hasApple: Set<int>) : int =
    let rec minTime' node parent graph =
        Map.find node graph
        |> List.fold
            (fun acc next ->
                if next = parent then
                    acc
                else
                    let childTime = minTime' next node graph

                    if childTime > 0 || (Set.contains next hasApple) then
                        acc + childTime + 2
                    else
                        acc)
            0

    let graph = edgesToGraph edges
    minTime' 0 -1 graph

// 8
minTime 7 [ (0, 1); (0, 2); (1, 4); (1, 5); (2, 3); (2, 6) ] (Set.ofList [ 2; 4; 5 ])

// 6
minTime 7 [ (0, 1); (0, 2); (1, 4); (1, 5); (2, 3); (2, 6) ] (Set.ofList [ 2; 5 ])
