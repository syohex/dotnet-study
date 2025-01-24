let eventualSafeNodes (graph: int list[]) : int list =
    let rec dfs node (visited: bool[]) (inPath: bool[]) =
        if inPath.[node] then
            true
        elif visited.[node] then
            false
        else
            inPath.[node] <- true
            visited.[node] <- true

            let ok =
                graph.[node] |> List.fold (fun ok next -> ok || dfs next visited inPath) false

            if ok then
                true
            else
                inPath.[node] <- false
                false

    let len = Array.length graph
    let visited: bool[] = Array.zeroCreate len
    let inPath: bool[] = Array.zeroCreate len
    seq { 0 .. len - 1 } |> Seq.iter (fun i -> dfs i visited inPath |> ignore)
    seq { 0 .. len - 1 } |> Seq.filter (fun i -> not <| inPath.[i]) |> Seq.toList

// [2,4,5,6]
eventualSafeNodes [| [ 1; 2 ]; [ 2; 3 ]; [ 5 ]; [ 0 ]; [ 5 ]; []; [] |]

// [4]
eventualSafeNodes [| [ 1; 2; 3; 4 ]; [ 1; 2 ]; [ 3; 4 ]; [ 0; 4 ]; [] |]

// [0,1,2,3,4]
eventualSafeNodes [| []; [ 0; 2; 3; 4 ]; [ 3 ]; [ 4 ]; [] |]
