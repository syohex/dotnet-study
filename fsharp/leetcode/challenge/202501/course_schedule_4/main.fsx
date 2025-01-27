let checkIfPrerequiste (numCourses: int) (prerequisites: (int * int) list) (queries: (int * int) list) : bool list =
    let rec toGraph prerequisites acc =
        match prerequisites with
        | [] -> acc
        | (s, e) :: t ->
            let v = Map.tryFind s acc |> Option.defaultValue []
            toGraph t (Map.add s (e :: v) acc)

    let rec bfs q init graph visited acc =
        match q with
        | [] -> acc
        | _ ->
            let visited = q |> List.fold (fun acc n -> Set.add n acc) visited

            let q =
                q
                |> List.fold
                    (fun acc n ->
                        match Map.tryFind n graph with
                        | None -> acc
                        | Some v ->
                            let valids = v |> List.filter (fun n -> not <| Set.contains n visited)
                            valids @ acc)
                    []

            let acc = q |> List.fold (fun acc n -> Set.add (init, n) acc) acc
            bfs q init graph visited acc

    let graph = toGraph prerequisites Map.empty

    let isReachable =
        seq { 0 .. numCourses - 1 }
        |> Seq.fold
            (fun acc n ->
                let acc = Set.add (n, n) acc
                bfs [ n ] n graph Set.empty acc)
            Set.empty

    queries |> List.map (fun q -> Set.contains q isReachable)

// [false, true]
checkIfPrerequiste 2 [ (1, 0) ] [ (0, 1); (1, 0) ]

// [false, false]
checkIfPrerequiste 2 [] [ (1, 0); (0, 1) ]

// [true, true]
checkIfPrerequiste 3 [ (1, 2); (1, 0); (2, 0) ] [ (1, 0); (1, 2) ]
