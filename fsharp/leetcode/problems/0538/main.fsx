type Tree =
    | Leaf
    | Node of int * Tree * Tree

let convertBST (root: Tree) =
    let rec convertBST' (node: Tree) (v: int) : (Tree * int) =
        match node with
        | Leaf -> Leaf, v
        | Node (n, left, right) ->
            let (right', rMax) = convertBST' right v
            let (left', lMax) = convertBST' left (n + rMax)
            Node(n + rMax, left', right'), lMax

    let t, _ = convertBST' root 0
    t


let tree1 =
    Node(
        4,
        Node(1, Node(0, Leaf, Leaf), Node(2, Leaf, Node(3, Leaf, Leaf))),
        Node(6, Node(5, Leaf, Leaf), Node(7, Leaf, Node(8, Leaf, Leaf)))
    )

// [30,36,21,36,35,26,15,null,null,null,33,null,null,null,8]
convertBST tree1

let tree2 = Node(0, Leaf, Node(1, Leaf, Leaf))
// [1, null, 1]
convertBST tree2
