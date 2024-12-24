open System

let toGraph edges =
    let rec toGraph' edges acc =
        match edges with
        | [] -> acc
        | (n1, n2) :: t ->
            let acc =
                match Map.tryFind n1 acc with
                | Some(v) -> Map.add n1 (n2 :: v) acc
                | None -> Map.add n1 [ n2 ] acc

            let acc =
                match Map.tryFind n2 acc with
                | Some(v) -> Map.add n2 (n1 :: v) acc
                | None -> Map.add n2 [ n1 ] acc

            toGraph' t acc

    toGraph' edges Map.empty

let getDiameter initNode graph =
    let rec getDiameter' q graph visited furtherestNode acc =
        match q with
        | [] -> acc, furtherestNode
        | _ ->
            let visited = q |> List.fold (fun acc node -> Set.add node acc) visited
            let furtherestNode = List.head q

            let q =
                q
                |> List.fold
                    (fun acc node ->
                        match Map.tryFind node graph with
                        | None -> acc
                        | Some(v) -> v @ acc)
                    []
                |> List.filter (fun node -> not <| Set.contains node visited)

            let acc = if List.isEmpty q then acc else acc + 1
            getDiameter' q graph visited furtherestNode acc

    getDiameter' [ initNode ] graph Set.empty initNode 0


let getLongtestDiameter graph =
    let _, furtherestNode = getDiameter 0 graph
    getDiameter furtherestNode graph |> fst

let minimumDiameterAfterMerge (edges1: (int * int) list) (edges2: (int * int) list) : int =
    let graph1 = toGraph edges1
    let graph2 = toGraph edges2

    let diameter1 = getLongtestDiameter graph1
    let diameter2 = getLongtestDiameter graph2
    let longest = max diameter1 diameter2
    let halfDiameter = double >> (fun n -> n / 2.0) >> Math.Ceiling >> int

    let mergedDiameter = halfDiameter diameter1 + halfDiameter diameter2 + 1

    max longest mergedDiameter

// 3
minimumDiameterAfterMerge [ (0, 1); (0, 2); (0, 3) ] [ (0, 1) ]

let edges = [ (0, 1); (0, 2); (0, 3); (2, 4); (2, 5); (3, 6); (2, 7) ]
// 5
minimumDiameterAfterMerge edges edges
