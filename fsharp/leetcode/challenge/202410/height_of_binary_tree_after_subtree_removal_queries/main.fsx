type Tree =
    | Leaf
    | Node of int * Tree * Tree

let treeNodeHeights (root: Tree) : Map<int, int> =
    let rec treeNodeHeights' node acc =
        match node with
        | Leaf -> -1, acc
        | Node(v, left, right) ->
            let leftHeight, acc = treeNodeHeights' left acc
            let rightHeight, acc = treeNodeHeights' right acc
            let height = 1 + max leftHeight rightHeight
            height, Map.add v height acc

    treeNodeHeights' root Map.empty |> snd

let treeQueries (root: Tree) (queries: int list) : int list =
    let rec heightRemoveNodes' node depth maxHeight treeHeights acc =
        match node with
        | Leaf -> acc
        | Node(v, left, right) ->
            let leftMaxHeight =
                match left with
                | Leaf -> depth
                | Node(v, _, _) -> depth + 1 + Map.find v treeHeights

            let rightMaxHeight =
                match right with
                | Leaf -> depth
                | Node(v, _, _) -> depth + 1 + Map.find v treeHeights

            let acc = Map.add v maxHeight acc

            let acc =
                heightRemoveNodes' left (depth + 1) (max maxHeight rightMaxHeight) treeHeights acc

            heightRemoveNodes' right (depth + 1) (max maxHeight leftMaxHeight) treeHeights acc

    let treeHeights = treeNodeHeights root
    let heights = heightRemoveNodes' root 0 0 treeHeights Map.empty

    queries |> List.map (fun query -> Map.find query heights)

let tree1 =
    Node(1, Node(3, Node(2, Leaf, Leaf), Leaf), Node(4, Node(6, Leaf, Leaf), Node(5, Leaf, Node(7, Leaf, Leaf))))
// [2]
treeQueries tree1 [ 4 ]

let tree2 =
    Node(
        5,
        Node(8, Node(2, Node(4, Leaf, Leaf), Node(6, Leaf, Leaf)), Node(1, Leaf, Leaf)),
        Node(9, Node(3, Leaf, Leaf), Node(7, Leaf, Leaf))
    )
// [3,2,3,2]
treeQueries tree2 [ 3; 2; 4; 8 ]

let tree3 =
    Node(1, Leaf, Node(5, Node(3, Node(2, Leaf, Leaf), Node(4, Leaf, Leaf)), Leaf))
// [1,0,3,3,3]
treeQueries tree3 [ 3; 5; 4; 2; 4 ]
