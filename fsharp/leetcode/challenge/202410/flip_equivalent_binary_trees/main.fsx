type Tree =
    | Leaf
    | Node of int * Tree * Tree

let flipEquiv (root1: Tree) (root2: Tree) : bool =
    let rec flipEquiv' t1 t2 =
        match t1, t2 with
        | Leaf, Leaf -> true
        | Node _, Leaf
        | Leaf, Node _ -> false
        | Node(v1, left1, right1), Node(v2, left2, right2) ->
            if v1 <> v2 then
                false
            else
                (flipEquiv' left1 left2 && flipEquiv' right1 right2)
                || (flipEquiv' left1 right2 && flipEquiv' right1 left2)

    flipEquiv' root1 root2

let tree11 =
    Node(
        1,
        Node(2, Node(4, Leaf, Leaf), Node(5, Node(7, Leaf, Leaf), Node(8, Leaf, Leaf))),
        Node(3, Node(6, Leaf, Leaf), Leaf)
    )

let tree12 =
    Node(
        1,
        Node(3, Leaf, Node(6, Leaf, Leaf)),
        Node(2, Node(4, Leaf, Leaf), Node(5, Node(8, Leaf, Leaf), Node(7, Leaf, Leaf)))
    )
// true
flipEquiv tree11 tree12

// true
flipEquiv Leaf Leaf

// false
flipEquiv Leaf (Node(1, Leaf, Leaf))
