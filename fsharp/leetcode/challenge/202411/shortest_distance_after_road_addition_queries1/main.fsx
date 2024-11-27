let shortestDistanceAfterQueries (n: int) (queries: (int * int) list) : int list =
    let rec bfs q steps goal (graph: (int list)[]) (visited: bool[]) =
        match q with
        | [] -> failwith "never reach here"
        | _ ->
            match List.tryFind ((=) goal) q with
            | Some _ -> steps
            | None ->
                q |> List.iter (fun node -> visited.[node] <- true)

                let q =
                    q
                    |> List.fold
                        (fun acc n ->
                            let nexts = graph.[n] |> List.filter (fun i -> not <| visited.[i])
                            nexts @ acc)
                        []

                bfs q (steps + 1) goal graph visited

    let rec shortestDistanceAfterQueries' queries n (graph: (int list)[]) acc =
        match queries with
        | [] -> List.rev acc
        | (s, e) :: t ->
            graph.[s] <- e :: graph.[s]
            let steps = bfs [ 0 ] 0 (n - 1) graph (Array.zeroCreate n)
            shortestDistanceAfterQueries' t n graph (steps :: acc)

    let graph =
        seq { 0 .. (n - 2) }
        |> Seq.fold
            (fun (acc: (int list)[]) i ->
                acc.[i] <- [ i + 1 ]
                acc)
            (Array.zeroCreate n)

    shortestDistanceAfterQueries' queries n graph []

// [3,2,1]
shortestDistanceAfterQueries 5 [ (2, 4); (0, 2); (0, 4) ]

// [1,1]
shortestDistanceAfterQueries 4 [ (0, 3); (0, 2) ]
