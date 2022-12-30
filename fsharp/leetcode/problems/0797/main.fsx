let allPathsSourceTarget (graph: int list []) : int list list =
    let target = graph.Length - 1

    let rec allPathsSourceTarget' q target (graph: int list []) acc =
        match q with
        | [] -> acc
        | (node, path, visited) :: t ->
            if node = target then
                allPathsSourceTarget' t target graph ((path |> List.rev) :: acc)
            else
                let visited' = Set.add node visited

                let q' =
                    graph.[node]
                    |> List.fold
                        (fun acc n ->
                            if Seq.contains n visited' then
                                acc
                            else
                                (n, (n :: path), visited') :: acc)
                        t

                allPathsSourceTarget' q' target graph acc

    let q = [ (0, [ 0 ], Set.empty) ]
    allPathsSourceTarget' q target graph []

// any order  [[0,1,3],[0,2,3]]
allPathsSourceTarget [| [ 1; 2 ]
                        [ 3 ]
                        [ 3 ]
                        [] |]

// any order  [[0,4],[0,3,4],[0,1,3,4],[0,1,2,3,4],[0,1,4]]
allPathsSourceTarget [| [ 4; 3; 1 ]
                        [ 3; 2; 4 ]
                        [ 3 ]
                        [ 4 ]
                        [] |]
