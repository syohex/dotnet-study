type Tree =
    | Leaf
    | Node of int * Tree * Tree

let inorderTraversal (root: Tree) : int list =
    let rec inorderTraversal' node acc =
        match node with
        | Leaf -> acc
        | Node (v, left, right) ->
            let acc' = inorderTraversal' left acc
            let acc'' = v :: acc'
            inorderTraversal' right acc''

    inorderTraversal' root [] |> List.rev

let tree1 =
    Node(1, Leaf, Node(2, Node(3, Leaf, Leaf), Leaf))
// [1; 3; 2]
inorderTraversal tree1

// []
inorderTraversal Leaf

// [1]
let tree3 = Node(1, Leaf, Leaf)
inorderTraversal tree3

let tree4 =
    Node(1, Node(2, Leaf, Leaf), Node(3, Leaf, Leaf))
// [2;1;3]
inorderTraversal tree4
