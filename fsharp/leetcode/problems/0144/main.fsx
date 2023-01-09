type Tree =
    | Leaf
    | Node of int * Tree * Tree

let preorderTraversal (root: Tree) : int list =
    let rec preorderTraversal' node acc =
        match node with
        | Leaf -> acc
        | Node (v, left, right) ->
            (v :: acc)
            |> preorderTraversal' left
            |> preorderTraversal' right

    preorderTraversal' root [] |> List.rev

let tree1 = Node(1, Leaf, Node(2, Node(3, Leaf, Leaf), Leaf))
// [1;2;3]
preorderTraversal tree1

// []
preorderTraversal Leaf

// [1]
preorderTraversal (Node(1, Leaf, Leaf))
