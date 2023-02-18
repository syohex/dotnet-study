type Tree =
    | Leaf
    | Node of int * Tree * Tree

let rec invertTree (root: Tree) : Tree =
    match root with
    | Leaf -> Leaf
    | Node (v, left, right) -> Node(v, invertTree right, invertTree left)

let tree1 =
    Node(4, Node(2, Node(1, Leaf, Leaf), Node(3, Leaf, Leaf)), Node(7, Node(6, Leaf, Leaf), Node(9, Leaf, Leaf)))

invertTree tree1

let tree2 = Node(2, Node(1, Leaf, Leaf), Node(3, Leaf, Leaf))
invertTree tree2

invertTree Leaf
