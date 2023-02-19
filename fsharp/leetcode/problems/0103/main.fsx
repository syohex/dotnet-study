open System

type Tree =
    | Leaf
    | Node of int * Tree * Tree

let zigzagLevelOrder (root: Tree) : int list list =
    let rec zigzagLevelOrder' node (depth: int) acc =
        match node with
        | Leaf -> acc, (depth - 1)
        | Node(v, left, right) ->
            let acc' =
                match Map.tryFind depth acc with
                | Some(vs) -> Map.add depth (v :: vs) acc
                | None -> Map.add depth [ v ] acc

            let acc'', depthLeft = zigzagLevelOrder' left (depth + 1) acc'
            let acc''', depthRight = zigzagLevelOrder' right (depth + 1) acc''

            acc''', Math.Max(depthLeft, depthRight)

    let acc, depth = zigzagLevelOrder' root 0 Map.empty

    seq { 0..depth }
    |> Seq.fold
        (fun ret i ->
            let vs = Map.find i acc
            if i % 2 = 0 then (List.sort vs) :: ret else vs :: ret)
        []
    |> List.rev

let tree1 =
    Node(3, Node(9, Leaf, Leaf), Node(20, Node(15, Leaf, Leaf), Node(7, Leaf, Leaf)))
// [[3],[20,9],[15,7]]
zigzagLevelOrder tree1

// [[1]]
zigzagLevelOrder (Node(1, Leaf, Leaf))

// []
zigzagLevelOrder Leaf
