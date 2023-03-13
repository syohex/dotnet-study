type Tree =
    | Leaf
    | Node of int * Tree * Tree

let isSymmetric (root: Tree) : bool =
    let rec isSymmetric' left right =
        match left, right with
        | Leaf, Leaf -> true
        | Leaf, _
        | _, Leaf -> false
        | Node(v1, left1, right1), Node(v2, left2, right2) ->
            v1 = v2 && isSymmetric' left1 right2 && isSymmetric' right1 left2

    isSymmetric' root root

let tree1 =
    Node(1, Node(2, Node(3, Leaf, Leaf), Node(4, Leaf, Leaf)), Node(2, Node(4, Leaf, Leaf), Node(3, Leaf, Leaf)))
// true
isSymmetric tree1

let tree2 =
    Node(1, Node(2, Leaf, Node(3, Leaf, Leaf)), Node(2, Leaf, Node(3, Leaf, Leaf)))
// false
isSymmetric tree2
