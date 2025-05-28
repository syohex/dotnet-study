let toGraph (edges: (int * int) list) (nodes: int) : (int list)[] =
    let rec toGraph' edges (acc: (int list)[]) =
        match edges with
        | [] -> acc
        | (n1, n2) :: t ->
            acc.[n1] <- n2 :: acc.[n1]
            acc.[n2] <- n1 :: acc.[n2]
            toGraph' t acc

    let acc = Array.init nodes (fun _ -> [])
    toGraph' edges acc

let rec countNodes (node: int) (k: int) (prev: int) (graph: (int list)[]) : int =
    if k < 0 then
        0
    else
        graph.[node]
        |> List.fold
            (fun acc n ->
                if n = prev then
                    acc
                else
                    acc + countNodes n (k - 1) node graph)
            1

let maxTargetNodes (edges1: (int * int) list) (edges2: (int * int) list) (k: int) : int list =
    let nodes1, nodes2 = List.length edges1 + 1, List.length edges2 + 1
    let graph1, graph2 = toGraph edges1 nodes1, toGraph edges2 nodes2

    let count1 =
        seq { 0 .. (nodes1 - 1) } |> Seq.map (fun n -> countNodes n k -1 graph1)

    let count2 =
        seq { 0 .. (nodes2 - 1) } |> Seq.map (fun n -> countNodes n (k - 1) -1 graph2)
    let max2 = Seq.max count2

    count1 |> Seq.map (fun n -> n + max2) |> Seq.toList

// [9,7,9,8,8]
maxTargetNodes [ (0, 1); (0, 2); (2, 3); (2, 4) ] [ (0, 1); (0, 2); (0, 3); (2, 7); (1, 4); (4, 5); (4, 6) ] 2

// [6,3,3,3,3]
maxTargetNodes [ (0, 1); (0, 2); (0, 3); (0, 4) ] [ (0, 1); (1, 2); (2, 3) ] 1
