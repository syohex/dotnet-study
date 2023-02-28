type Tree =
    | Leaf
    | Node of int * Tree * Tree

let findDuplicateSubtrees (root: Tree) : Tree list =
    let rec findDuplicateSubtrees' node acc ret =
        match node with
        | Leaf -> acc, ret
        | Node(_, left, right) ->
            let acc', ret' = findDuplicateSubtrees' left acc ret
            let acc'', ret'' = findDuplicateSubtrees' right acc' ret'

            match Map.tryFind node acc'' with
            | Some(v) ->
                if v = 1 then
                    Map.add node (v + 1) acc'', node :: ret''
                else
                    acc'', ret''
            | None -> Map.add node 1 acc'', ret''

    findDuplicateSubtrees' root Map.empty [] |> snd

let tree1 =
    Node(1, Node(2, Node(4, Leaf, Leaf), Leaf), Node(3, Node(2, Node(4, Leaf, Leaf), Leaf), Node(4, Leaf, Leaf)))

// [[2,4],[4]]
findDuplicateSubtrees tree1

let tree2 = Node(2, Node(1, Leaf, Leaf), Node(1, Leaf, Leaf))
// [[1]]
findDuplicateSubtrees tree2

let tree3 =
    Node(2, Node(2, Node(3, Leaf, Leaf), Leaf), Node(2, Node(3, Leaf, Leaf), Leaf))
// [[2, 3], [3]]
findDuplicateSubtrees tree3
