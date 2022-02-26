let convertGraph (graph: int list list) : int list [] = graph |> List.toArray

let nextCandidates (node: int) (mask: int) (graph: int list []) (seen: Set<int> []) : (int * int) list =
    let rec nextCandidates' cands acc (seen: Set<int> []) =
        match cands with
        | [] -> acc
        | x :: xs ->
            let newMask = mask ||| (1 <<< x)

            if Set.contains newMask seen.[x] then
                nextCandidates' xs acc seen
            else
                seen.[x] <- Set.add newMask seen.[x]
                nextCandidates' xs ((x, newMask) :: acc) seen

    nextCandidates' graph.[node] [] seen


let shortestPathLength (graph: int list list) : int =
    let rec shortestPathLength'
        (steps: int)
        (candidates: (int * int) list)
        (graph: int list [])
        (endMask: int)
        (seen: Set<int> [])
        : int =
        let nexts =
            candidates
            |> List.fold
                (fun acc (node, mask) ->
                    let nexts = nextCandidates node mask graph seen
                    nexts @ acc)
                []

        if (nexts
            |> List.exists (fun (_, mask) -> mask = endMask)) then
            steps
        else
            shortestPathLength' (steps + 1) nexts graph endMask seen

    let g = convertGraph graph
    let endMask = (1 <<< g.Length) - 1

    let candidates =
        seq { 0 .. (g.Length - 1) }
        |> Seq.map (fun n -> (n, (1 <<< n)))
        |> Seq.toList

    let seen =
        Array.init g.Length (fun i -> Set.empty |> Set.add (1 <<< i))

    shortestPathLength' 1 candidates g endMask seen

// 4
let graph1 = [ [ 1; 2; 3 ]; [ 0 ]; [ 0 ]; [ 0 ] ]
shortestPathLength graph1

// 4
let graph2 =
    [ [ 1 ]
      [ 0; 2; 4 ]
      [ 1; 3; 4 ]
      [ 2 ]
      [ 1; 2 ] ]

shortestPathLength graph2
