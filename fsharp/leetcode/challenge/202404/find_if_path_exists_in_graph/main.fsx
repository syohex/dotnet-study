let validPath (n: int) (edges: (int * int) list) (source: int) (destination: int) : bool =
    let rec toGraph edges (graph: (int list)[]) =
        match edges with
        | [] -> graph
        | (s, e) :: t ->
            graph.[e] <- s :: graph.[e]
            graph.[s] <- e :: graph.[s]

            toGraph t graph

    let rec validPath' q (graph: (int list)[]) visited =
        match q with
        | [] -> false
        | _ ->
            match List.tryFind ((=) destination) q with
            | Some(_) -> true
            | None ->
                let visited' = q |> List.fold (fun acc n -> Set.add n acc) visited

                let q' =
                    q
                    |> List.fold
                        (fun acc node ->
                            let nexts = graph.[node] |> List.filter (fun n -> not <| Set.contains n visited')
                            nexts @ acc)
                        []

                validPath' q' graph visited'

    let graph = toGraph edges (Array.zeroCreate n)
    validPath' [ source ] graph Set.empty

let edges1 = [ (0, 1); (1, 2); (2, 0) ]
// true
validPath 3 edges1 0 2

let edges2 = [ (0, 1); (0, 2); (3, 5); (5, 4); (4, 3) ]
// false
validPath 6 edges2 0 5
