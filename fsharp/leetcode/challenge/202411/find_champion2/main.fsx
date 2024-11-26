let findChampion (n: int) (edges: (int * int) list) : int =
    let rec toGraph edges acc =
        match edges with
        | [] -> acc
        | (s, e) :: t ->
            let acc =
                match Map.tryFind s acc with
                | Some(v) -> Map.add s (e :: v) acc
                | None -> Map.add s [ e ] acc

            toGraph t acc

    let rec dfs node graph cache =
        match Map.tryFind node cache with
        | Some(v) -> v, cache
        | None ->
            match Map.tryFind node graph with
            | None -> 1, Map.add node 1 cache
            | Some(v) ->
                let ret, cache =
                    v
                    |> List.fold
                        (fun (acc, cache) n ->
                            match Map.tryFind n cache with
                            | Some _ -> acc, cache
                            | None ->
                                let ret, cache = dfs n graph cache
                                acc + ret, cache)
                        (1, cache)

                ret, Map.add node ret cache

    let rec findChampion' i n graph =
        if i >= n then
            -1
        else
            let ret, _ = dfs i graph Map.empty
            if ret = n then i else findChampion' (i + 1) n graph

    let graph = toGraph edges Map.empty
    findChampion' 0 n graph

// 0
findChampion 3 [ (0, 1); (1, 2) ]

// -1
findChampion 4 [ (0, 2); (1, 3); (1, 2) ]

// 1
findChampion 2 [ (1, 0) ]

// -1
findChampion 4 [ (0, 1); (2, 0); (2, 1) ]

// 0
findChampion 3 [ (0, 1); (1, 2); (0, 2) ]
