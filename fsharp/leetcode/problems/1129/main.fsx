let edgesToGraph (edges: (int * int) list) : Map<int, int list> =
    let rec edgesToGraph' edges acc =
        match edges with
        | [] -> acc
        | (src, dest) :: t ->
            match Map.tryFind src acc with
            | Some (v) -> edgesToGraph' t (Map.add src (dest :: v) acc)
            | None -> edgesToGraph' t (Map.add src [ dest ] acc)

    edgesToGraph' edges Map.empty

let rec shortestAlternatingPaths'
    (q: (int * bool) list)
    steps
    (redGraph: Map<int, int list>)
    (blueGraph: Map<int, int list>)
    (redVisited: bool [])
    (blueVisited: bool [])
    (acc: int [])
    : int [] =
    match q with
    | [] -> acc
    | _ ->
        q
        |> List.iter (fun (n, _) -> if acc.[n] = -1 then acc.[n] <- steps)

        let q' =
            q
            |> List.fold
                (fun acc (node, isRed) ->
                    let visited =
                        if isRed then
                            blueVisited
                        else
                            redVisited

                    let graph = if isRed then blueGraph else redGraph

                    match Map.tryFind node graph with
                    | None -> acc
                    | Some (nexts) ->
                        let nexts' =
                            nexts
                            |> List.filter (fun n -> not visited.[n])
                            |> List.map (fun n -> n, not isRed)

                        nexts'
                        |> List.iter (fun (n, _) -> visited.[n] <- true)

                        nexts' @ acc)
                []

        shortestAlternatingPaths' q' (steps + 1) redGraph blueGraph redVisited blueVisited acc


let shortestAlternatingPaths (n: int) (redEdges: (int * int) list) (blueEdges: (int * int) list) : int [] =
    let redGraph = edgesToGraph redEdges
    let blueGraph = edgesToGraph blueEdges

    let redVisited = Array.init n (fun _ -> false)
    let blueVisited = Array.init n (fun _ -> false)

    let redHasZero = Map.tryFind 0 redGraph |> Option.isSome
    let blueHasZero = Map.tryFind 0 blueGraph |> Option.isSome

    let ret = Array.init n (fun _ -> -1)
    ret.[0] <- 0

    let q =
        match redHasZero, blueHasZero with
        | true, true -> [ (0, false); (0, true) ]
        | true, false -> [ (0, false) ]
        | false, true -> [ 0, true ]
        | false, false -> []

    shortestAlternatingPaths' q 0 redGraph blueGraph redVisited blueVisited ret


// [0, 1, -1]
shortestAlternatingPaths 3 [ (0, 1); (1, 2) ] []

// [0, 1, -1]
shortestAlternatingPaths 3 [ (0, 1) ] [ (2, 1) ]

// [0, -1, -1]
shortestAlternatingPaths 3 [ (1, 0) ] [ (2, 1) ]

// [0, 1, 2, 3, 7]
shortestAlternatingPaths 5 [ (0, 1); (1, 2); (2, 3); (3, 4) ] [ (1, 2); (2, 3); (3, 1) ]
