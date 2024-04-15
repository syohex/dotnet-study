type Tree =
    | Leaf
    | Node of int * Tree * Tree

let sumNumbers (root: Tree) : int =
    let rec sumNumbers' node total =
        match node with
        | Leaf -> 0
        | Node(v, Leaf, Leaf) -> 10 * total + v
        | Node(v, left, right) ->
            let total' = 10 * total + v
            (sumNumbers' left total') + (sumNumbers' right total')

    sumNumbers' root 0

let tree1 = Node(1, Node(2, Leaf, Leaf), Node(3, Leaf, Leaf))
// 25
sumNumbers tree1

let tree2 =
    Node(4, Node(9, Node(5, Leaf, Leaf), Node(1, Leaf, Leaf)), Node(0, Leaf, Leaf))
// 1026
sumNumbers tree2
