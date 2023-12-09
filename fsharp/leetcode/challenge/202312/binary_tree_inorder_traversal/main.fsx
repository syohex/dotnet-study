type Tree =
    | Leaf
    | Node of int * Tree * Tree

let inorderTraversal (root: Tree) : int list =
    let rec inorderTraversal' node acc =
        match node with
        | Leaf -> acc
        | Node(v, left, right) ->
            let acc' = inorderTraversal' left acc
            inorderTraversal' right (v :: acc')

    inorderTraversal' root [] |> List.rev

let tree1 = Node(1, Leaf, Node(2, Node(3, Leaf, Leaf), Leaf))
// [1,3,2]
inorderTraversal tree1

// []
inorderTraversal Leaf

// [1]
inorderTraversal (Node(1, Leaf, Leaf))
