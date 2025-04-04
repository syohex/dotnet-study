type Tree =
    | Leaf
    | Node of int * Tree * Tree

let lcaDeepestLeaves (root: Tree) : Tree =
    let rec lcaDeepestLeaves' node depth =
        match node with
        | Leaf -> Leaf, depth
        | Node(_, left, right) ->
            let left', leftDepth = lcaDeepestLeaves' left (depth + 1)
            let right', rightDepth = lcaDeepestLeaves' right (depth + 1)

            if leftDepth = rightDepth then node, leftDepth
            elif leftDepth < rightDepth then right', rightDepth
            else left', leftDepth

    lcaDeepestLeaves' root 0 |> fst

let tree1 =
    Node(
        3,
        Node(5, Node(6, Leaf, Leaf), Node(2, Node(7, Leaf, Leaf), Node(4, Leaf, Leaf))),
        Node(1, Node(0, Leaf, Leaf), Node(8, Leaf, Leaf))
    )
// [2,7,4]
lcaDeepestLeaves tree1

let tree2 = Node(1, Leaf, Leaf)
// [1]
lcaDeepestLeaves tree2

let tree3 = Node(0, Node(1, Node(2, Leaf, Leaf), Leaf), Node(3, Leaf, Leaf))
// [2]
lcaDeepestLeaves tree3
