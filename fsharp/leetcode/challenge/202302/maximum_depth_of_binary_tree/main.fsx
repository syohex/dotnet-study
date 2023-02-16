type Tree =
    | Leaf
    | Node of int * Tree * Tree

let maxDepth (root: Tree) : int =
    let rec maxDepth' q acc =
        match q with
        | [] -> acc
        | _ ->
            let acc' = q |> List.map snd |> List.max

            let q' =
                q
                |> List.fold
                    (fun acc (node, depth) ->
                        match node with
                        | Leaf -> acc
                        | Node(_, left, right) ->
                            let acc' =
                                match left with
                                | Leaf -> acc
                                | _ -> (left, depth + 1) :: acc

                            match right with
                            | Leaf -> acc'
                            | _ -> (right, depth + 1) :: acc')
                    []

            maxDepth' q' acc'

    match root with
    | Leaf -> 0
    | _ -> maxDepth' [ (root, 1) ] 0


// 0
maxDepth Leaf

let tree1 =
    Node(3, Node(9, Leaf, Leaf), Node(20, Node(15, Leaf, Leaf), Node(7, Leaf, Leaf)))
// 3
maxDepth tree1

let tree2 = Node(1, Leaf, Node(2, Leaf, Leaf))
// 2
maxDepth tree2
