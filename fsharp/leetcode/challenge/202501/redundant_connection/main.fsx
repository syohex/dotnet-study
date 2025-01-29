let findRedundantConnection (edges: (int * int) list) : int * int =
    let rec isConnected node1 node2 graph visited =
        if node1 = node2 then
            true
        else
            let visited = Set.add node1 visited
            let nexts = Map.tryFind node1 graph |> Option.defaultValue []

            nexts
            |> List.filter (fun n -> not <| Set.contains n visited)
            |> List.fold (fun ok node -> if ok then ok else isConnected node node2 graph visited) false

    let addEdge node1 node2 graph =
        match Map.tryFind node1 graph with
        | Some(v) -> Map.add node1 (node2 :: v) graph
        | None -> Map.add node1 [ node2 ] graph

    let rec findRedundantConnection' edges graph =
        match edges with
        | [] -> failwith "never reach here"
        | (s, e) :: t ->
            if isConnected s e graph Set.empty then
                s, e
            else
                let graph = graph |> addEdge s e |> addEdge e s
                findRedundantConnection' t graph

    findRedundantConnection' edges Map.empty

// [2,3]
findRedundantConnection [ (1, 2); (1, 3); (2, 3) ]

// [1,4]
findRedundantConnection [ (1, 2); (2, 3); (3, 4); (1, 4); (1, 5) ]
