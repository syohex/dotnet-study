type Tree =
    | Leaf
    | Node of int * Tree * Tree

let flatten (root: Tree) : Tree =
    let rec collectNodes node acc =
        match node with
        | Leaf -> acc |> List.rev
        | Node (v, left, right) ->
            (collectNodes left (v :: acc))
            @ (collectNodes right [])

    let rec nodesToFlattenTree nodes =
        match nodes with
        | [] -> Leaf
        | h :: t -> Node(h, Leaf, nodesToFlattenTree t)

    collectNodes root [] |> nodesToFlattenTree

let tree1 =
    Node(1, Node(2, Node(3, Leaf, Leaf), Node(4, Leaf, Leaf)), Node(5, Leaf, Node(6, Leaf, Leaf)))
// [1,null,2,null,3,null,4,null,5,null,6]
flatten tree1

// []
flatten Leaf

// [0]
flatten (Node(0, Leaf, Leaf))
