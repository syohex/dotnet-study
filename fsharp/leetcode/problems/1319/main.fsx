let toGraph (connections: (int * int) list) : Map<int, int list> =
    let rec toGraph' connections acc =
        match connections with
        | [] -> acc
        | (a, b) :: t ->
            let acc' =
                match Map.tryFind a acc with
                | None -> Map.add a [ b ] acc
                | Some(v) -> Map.add a (b :: v) acc

            let acc'' =
                match Map.tryFind b acc' with
                | None -> Map.add b [ a ] acc'
                | Some(v) -> Map.add b (a :: v) acc'

            toGraph' t acc''

    toGraph' connections Map.empty

let makeConnected (n: int) (connections: (int * int) list) : int =
    let rec bfs q graph visited =
        match q with
        | [] -> visited
        | h :: t ->
            let visited' = Set.add h visited

            match Map.tryFind h graph with
            | None -> bfs t graph visited'
            | Some(nexts) ->
                let q' =
                    nexts
                    |> List.fold (fun acc n -> if Set.contains n visited' then acc else n :: acc) t

                bfs q' graph visited'

    let rec makeConnected' i n graph visited islands =
        if i >= n then
            islands - 1
        else if Set.contains i visited then
            makeConnected' (i + 1) n graph visited islands
        else
            let visited' = bfs [ i ] graph visited
            makeConnected' (i + 1) n graph visited' (islands + 1)

    let len = List.length connections

    if len < n - 1 then
        -1
    else
        let graph = toGraph connections
        makeConnected' 0 n graph Set.empty 0

// 1
makeConnected 4 [ (0, 1); (0, 2); (1, 2) ]

// 2
makeConnected 6 [ (0, 1); (0, 2); (0, 3); (1, 2); (1, 3) ]

// -1
makeConnected 6 [ (0, 1); (0, 2); (0, 3); (1, 2) ]
