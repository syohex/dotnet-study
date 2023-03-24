let toGraph (connections: (int * int) list) : Map<int, (int * int) list> =
    let rec toGraph' connections acc =
        match connections with
        | [] -> acc
        | (a, b) :: t ->
            let acc' =
                match Map.tryFind a acc with
                | None -> Map.add a [ (b, 1) ] acc
                | Some(v) -> Map.add a ((b, 1) :: v) acc

            let acc'' =
                match Map.tryFind b acc with
                | None -> Map.add b [ (a, 0) ] acc'
                | Some(v) -> Map.add b ((a, 0) :: v) acc'

            toGraph' t acc''

    toGraph' connections Map.empty

let minReorder (_: int) (connections: (int * int) list) : int =
    let rec minReorder' q graph visited ret =
        match q with
        | [] -> ret
        | h :: t ->
            let visited' = Set.add h visited

            match Map.tryFind h graph with
            | None -> minReorder' t graph visited' ret
            | Some(v) ->
                let q', ret' =
                    v
                    |> List.fold
                        (fun (acc, ret) (next, direction) ->
                            if Set.contains next visited then
                                acc, ret
                            else
                                next :: acc, ret + direction)
                        (t, ret)

                minReorder' q' graph visited' ret'

    let graph = toGraph connections
    minReorder' [ 0 ] graph Set.empty 0

// 3
minReorder 6 [ (0, 1); (1, 3); (2, 3); (4, 0); (4, 5) ]
// 2
minReorder 5 [ (1, 0); (1, 2); (3, 2); (3, 4) ]
// 0
minReorder 6 [ (1, 0); (2, 0) ]
