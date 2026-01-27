type Edge = { node: int; cost: int }

let toGraph (edges: (int * int * int) list) : Map<int, Edge list> =
    let rec f edges acc =
        match edges with
        | [] -> acc
        | (from, to_, cost) :: t ->
            let edge = { node = to_; cost = cost }

            let acc =
                match Map.tryFind from acc with
                | Some v -> Map.add from (edge :: v) acc
                | None -> Map.add from [ edge ] acc

            let reverseEdge = { node = from; cost = cost * 2 }

            let acc =
                match Map.tryFind to_ acc with
                | Some v -> Map.add to_ (reverseEdge :: v) acc
                | None -> Map.add to_ [ reverseEdge ] acc

            f t acc

    f edges Map.empty

let minCost (n: int) (edges: (int * int * int) list) : int =
    let rec f q (graph: Map<int, Edge list>) (costs: Map<int, int>) =
        match q with
        | [] -> Map.tryFind (n - 1) costs |> Option.defaultValue -1
        | (node, cost) :: t ->
            let nexts = Map.tryFind node graph |> Option.defaultValue []

            let q, costs =
                nexts
                |> List.fold
                    (fun (q, costs) { node = nextNode; cost = nextCost } ->
                        let newCost = cost + nextCost

                        match Map.tryFind nextNode costs with
                        | None -> (nextNode, newCost) :: q, Map.add nextNode newCost costs
                        | Some v ->
                            if newCost < v then
                                (nextNode, newCost) :: q, Map.add nextNode newCost costs
                            else
                                q, costs)
                    (t, costs)

            f q graph costs


    let graph = toGraph edges
    f [ (0, 0) ] graph Map.empty

// 5
minCost 4 [ (0, 1, 3); (3, 1, 1); (2, 3, 4); (0, 2, 2) ]

// 3
minCost 4 [ (0, 2, 1); (2, 1, 1); (1, 3, 1); (2, 3, 3) ]

// -1
minCost 4 [ (2, 3, 25); (2, 1, 18); (3, 1, 2) ]
