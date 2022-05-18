open System

let toGraph (conns: (int * int) list) : Map<int, int list> =
    conns
    |> List.fold
        (fun acc (src, dest) ->
            let acc' =
                match Map.tryFind src acc with
                | None -> Map.add src [ dest ] acc
                | Some (v) -> Map.add src (dest :: v) acc

            match Map.tryFind dest acc' with
            | None -> Map.add dest [ src ] acc'
            | Some (v) -> Map.add dest (src :: v) acc')
        Map.empty

let criticalConnections (n: int) (connections: (int * int) list) : (int * int) list =
    let rec f
        (node: int)
        (parent: int)
        (rank: int)
        (graph: Map<int, int list>)
        (visited: Map<int, int>)
        (acc: (int * int) list)
        : (int * Map<int, int> * (int * int) list) =
        match Map.tryFind node visited with
        | Some (v) -> v, visited, acc
        | None ->
            let visited' = Map.add node rank visited

            Map.find node graph
            |> List.fold
                (fun (minRank, visited, acc) next ->
                    if next = parent then
                        minRank, visited, acc
                    else
                        let rank', visited', acc' = f next node (rank + 1) graph visited acc

                        if rank' = rank + 1 then
                            Math.Min(minRank, rank'), visited', ((node, next) :: acc')
                        else
                            Math.Min(minRank, rank'), visited', acc')
                (rank, visited', acc)


    let graph = toGraph connections
    let _, _, ret = f 0 -1 0 graph Map.empty []
    ret

let connections1 = [ (0, 1); (1, 2); (2, 0); (1, 3) ]
// [[1,3]]
criticalConnections 4 connections1

// [[0,1]]
criticalConnections 2 [ (0, 1) ]
