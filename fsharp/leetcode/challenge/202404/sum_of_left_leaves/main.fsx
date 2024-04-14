type Tree =
    | Leaf
    | Node of int * Tree * Tree

let sumOfLeftLeaves (root: Tree) : int =
    let rec sumOfLeftLeaves' node isLeft =
        match node with
        | Leaf -> 0
        | Node(v, Leaf, Leaf) when isLeft -> v
        | Node(_, left, right) -> (sumOfLeftLeaves' left true) + (sumOfLeftLeaves' right false)

    sumOfLeftLeaves' root false

let tree1 =
    Node(3, Node(9, Leaf, Leaf), Node(20, Node(15, Leaf, Leaf), Node(7, Leaf, Leaf)))
// 24
sumOfLeftLeaves tree1

let tree2 = Node(1, Leaf, Leaf)
// 1
sumOfLeftLeaves tree2
