type Tree =
    | Leaf
    | Node of int * Tree * Tree

let hasPathSum (root: Tree) (targetSum: int) : bool =
    let rec hasPathSum' node sum targetSum =
        match node with
        | Leaf -> sum = targetSum
        | Node (v, left, right) ->
            hasPathSum' left (sum + v) targetSum
            || hasPathSum' right (sum + v) targetSum

    match root with
    | Leaf -> false
    | _ -> hasPathSum' root 0 targetSum


let tree1 =
    Node(
        5,
        Node(4, Node(11, Node(7, Leaf, Leaf), Node(2, Leaf, Leaf)), Leaf),
        Node(8, Node(13, Leaf, Leaf), Node(4, Leaf, Node(1, Leaf, Leaf)))
    )
// true
hasPathSum tree1 22

let tree2 = Node(1, Node(2, Leaf, Leaf), Node(3, Leaf, Leaf))
// false
hasPathSum tree2 5

// flase
hasPathSum Leaf 0
