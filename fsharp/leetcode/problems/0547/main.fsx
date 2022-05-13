let findCircleNum (isConnected: int list []) : int =
    let rec moveNext nodes (isConnected: int list []) visited =
        match nodes with
        | [] -> visited
        | node :: rest ->
            let nodes' =
                isConnected.[node]
                |> List.mapi (fun i n -> i, n)
                |> List.filter (fun (i, n) -> n = 1 && (Set.contains i visited |> not))
                |> List.fold (fun acc (i, _) -> i :: acc) rest

            let visited' =
                nodes'
                |> List.fold (fun acc n -> Set.add n acc) visited

            moveNext nodes' isConnected visited'

    let rec findCircleNum' i (isConnected: int list []) visited ret =
        if i = isConnected.Length then
            ret
        else if Set.contains i visited then
            findCircleNum' (i + 1) isConnected visited ret
        else
            let visited' =
                moveNext [ i ] isConnected (Set.add i visited)

            findCircleNum' (i + 1) isConnected visited' (ret + 1)

    findCircleNum' 0 isConnected Set.empty 0

// 2
findCircleNum [| [ 1; 1; 0 ]
                 [ 1; 1; 0 ]
                 [ 0; 0; 1 ] |]

// 3
findCircleNum [| [ 1; 0; 0 ]
                 [ 0; 1; 0 ]
                 [ 0; 0; 1 ] |]

// 3
findCircleNum [| [ 1; 1; 0; 0; 0; 0 ]
                 [ 1; 1; 0; 0; 0; 0 ]
                 [ 0; 0; 1; 1; 1; 0 ]
                 [ 0; 0; 1; 1; 0; 0 ]
                 [ 0; 0; 1; 0; 1; 0 ]
                 [ 0; 0; 0; 0; 0; 1 ] |]
