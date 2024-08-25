type Tree =
    | Leaf
    | Node of int * Tree * Tree

let postorderTraversal (root: Tree) : int list =
    let rec postOrderTraversal' node acc =
        match node with
        | Leaf -> acc
        | Node(v, left, right) ->
            let acc = postOrderTraversal' left acc
            let acc = postOrderTraversal' right acc
            v :: acc

    postOrderTraversal' root [] |> List.rev

let tree1 = Node(1, Leaf, Node(2, Node(3, Leaf, Leaf), Leaf))
// [3,2,1]
postorderTraversal tree1

// []
postorderTraversal Leaf

let tree3 = Node(1, Leaf, Leaf)
// 1
postorderTraversal tree3
