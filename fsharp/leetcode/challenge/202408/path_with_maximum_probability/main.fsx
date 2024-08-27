#r "nuget:FSharpx.Collections"
open FSharpx.Collections

let toGraph (edges: (int * int) list) (succProb: double list) : Map<int, (int * double) list> =
    let rec toGraph' edges succProb acc =
        match edges, succProb with
        | [], [] -> acc
        | _, []
        | [], _ -> failwith "never reach here"
        | (node1, node2) :: t1, score :: t2 ->
            let acc =
                match Map.tryFind node1 acc with
                | Some(v) -> Map.add node1 ((node2, score) :: v) acc
                | None -> Map.add node1 [ (node2, score) ] acc

            let acc =
                match Map.tryFind node2 acc with
                | Some(v) -> Map.add node2 ((node1, score) :: v) acc
                | None -> Map.add node2 [ (node1, score) ] acc

            toGraph' t1 t2 acc

    toGraph' edges succProb Map.empty

let maxProbability (n: int) (edges: (int * int) list) (succProb: double list) (startNode: int) (endNode: int) : double =
    let rec maxProbability' (q: IPriorityQueue<int * double>) graph (maxScores: double[]) =
        if PriorityQueue.isEmpty q then
            maxScores.[endNode]
        else
            let (node, currentScore), q = PriorityQueue.pop q
            let nexts = Map.tryFind node graph |> Option.defaultValue []

            let q =
                nexts
                |> List.fold
                    (fun acc (next, nextScore) ->
                        let score = currentScore * nextScore

                        if score > maxScores.[next] then
                            maxScores.[next] <- score
                            PriorityQueue.insert (next, nextScore) acc
                        else
                            acc)
                    q

            maxProbability' q graph maxScores

    let graph = toGraph edges succProb
    let maxScores = Array.zeroCreate n
    let q = PriorityQueue.empty true |> PriorityQueue.insert (0, 1.0)
    maxProbability' q graph maxScores

// 0.25
maxProbability 3 [ (0, 1); (1, 2); (0, 2) ] [ 0.5; 0.5; 0.2 ] 0 2

// 0.3
maxProbability 3 [ (0, 1); (1, 2); (0, 2) ] [ 0.5; 0.5; 0.3 ] 0 2

// 0.0
maxProbability 3 [ (0, 1) ] [ 0.5 ] 0 2
