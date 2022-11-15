type Tree =
    | Leaf
    | Node of int * Tree * Tree

let rec countNodes (root: Tree) : int =
    match root with
    | Leaf -> 0
    | Node (_, left, right) -> 1 + countNodes left + countNodes right

let tree1 =
    Node(1, Node(2, Node(4, Leaf, Leaf), Node(5, Leaf, Leaf)), Node(3, Node(6, Leaf, Leaf), Leaf))
// 6
countNodes tree1

// 0
countNodes Leaf

// 1
countNodes (Node(1, Leaf, Leaf))
