let edgesToGraph (n: int) (edges: (int * int) list) : int list[] =
    let rec edgesToGraph' edges (graph: int list[]) =
        match edges with
        | [] -> graph
        | (s, e) :: t ->
            graph.[s] <- (e :: graph.[s])
            graph.[e] <- (s :: graph.[e])
            edgesToGraph' t graph

    let graph = Array.init n (fun _ -> [])
    edgesToGraph' edges graph

let maxKDivisibleComponents (n: int) (edges: (int * int) list) (values: int list) (k: int) : int =
    let graph = edgesToGraph n edges
    let values = Array.ofList values

    let rec maxKDivisibleComponents' node prev =
        let sum, acc =
            graph.[node]
            |> List.fold
                (fun (sum, acc) next ->
                    if next <> prev then
                        let sum', acc' = maxKDivisibleComponents' next node
                        (sum + sum') % k, acc + acc'
                    else
                        sum, acc)
                (values.[node], 0)

        if sum % k = 0 then sum, acc + 1 else sum, acc

    maxKDivisibleComponents' 0 -1 |> snd

let edges1 = [ (0, 2); (1, 2); (1, 3); (2, 4) ]
// 2
maxKDivisibleComponents 5 edges1 [ 1; 8; 1; 4; 4 ] 6

let edges2 = [ (0, 1); (0, 2); (1, 3); (1, 4); (2, 5); (2, 6) ]
// 3
maxKDivisibleComponents 7 edges2 [ 3; 0; 6; 1; 5; 2; 1 ] 3
