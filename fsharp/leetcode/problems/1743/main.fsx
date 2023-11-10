let toGraph (pairs: (int * int) list) : Map<int, int list> =
    let rec toGraph' pairs acc =
        match pairs with
        | [] -> acc
        | (n1, n2) :: t ->
            let acc' =
                match Map.tryFind n1 acc with
                | Some(v) -> Map.add n1 (n2 :: v) acc
                | None -> Map.add n1 [ n2 ] acc

            let acc'' =
                match Map.tryFind n2 acc' with
                | Some(v) -> Map.add n2 (n1 :: v) acc'
                | None -> Map.add n2 [ n1 ] acc'

            toGraph' t acc''

    toGraph' pairs Map.empty

let findRoot (graph: Map<int, int list>) : int =
    graph |> Map.findKey (fun _ v -> List.length v = 1)

let restoreArray (adjacentPairs: (int * int) list) : int list =
    let rec restoreArray' node graph visited acc =
        let visited' = Set.add node visited
        let acc' = node :: acc

        match Map.tryFind node graph with
        | None -> failwith "never reach here"
        | Some(nodes) ->
            match List.tryFind (fun n -> not <| Set.contains n visited) nodes with
            | Some(next) -> restoreArray' next graph visited' acc'
            | None -> List.rev acc'

    let graph = toGraph adjacentPairs
    let root = findRoot graph
    restoreArray' root graph Set.empty []

// [1;2;3;4] or [4;3;2;1]
restoreArray [ (2, 1); (3, 4); (3, 2) ]

// [-2;4;1;-3] or [-3;1;4;-2]
restoreArray [ (4, -2); (1, 4); (-3, 1) ]

// [-1000;1000] or [1000;-1000]
restoreArray [ (1000, -1000) ]
