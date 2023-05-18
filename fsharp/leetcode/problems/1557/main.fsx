let findSmallestSetOfVertices (n: int) (edges: (int * int) list) : int list =
    let reachables = edges |> List.map snd |> Set.ofList

    seq { 0 .. (n - 1) }
    |> Seq.filter (fun i -> Set.contains i reachables |> not)
    |> List.ofSeq

// [0, 3]
findSmallestSetOfVertices 6 [ (0, 1); (0, 2); (2, 5); (3, 4); (4, 2) ]

// [0, 2, 3]
findSmallestSetOfVertices 5 [ (0, 1); (2, 1); (3, 1); (1, 4); (2, 4) ]
