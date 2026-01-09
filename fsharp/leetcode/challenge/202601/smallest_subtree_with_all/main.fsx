type Tree =
    | Leaf
    | Node of int * Tree * Tree

let subtreeWithAllDeepest (root: Tree) : Tree =
    let rec f node =
        match node with
        | Leaf -> Leaf, 0
        | Node(_, left, right) ->
            let n1, v1 = f left
            let n2, v2 = f right

            if v1 > v2 then n1, v1 + 1
            elif v1 < v2 then n2, v2 + 1
            else node, v1 + 1

    f root |> fst

let tree1 =
    Node(
        3,
        Node(5, Node(6, Leaf, Leaf), Node(2, Node(7, Leaf, Leaf), Node(4, Leaf, Leaf))),
        Node(1, Node(0, Leaf, Leaf), Node(8, Leaf, Leaf))
    )
// [2, 7, 4]
subtreeWithAllDeepest tree1

let tree2 = Node(1, Leaf, Leaf)
// [1]
subtreeWithAllDeepest tree2

let tree3 = Node(0, Node(1, Leaf, Node(2, Leaf, Leaf)), Node(3, Leaf, Leaf))
// [2]
subtreeWithAllDeepest tree3
