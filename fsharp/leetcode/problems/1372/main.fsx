open System

type Tree =
    | Leaf
    | Node of int * Tree * Tree

let longestZigZa (root: Tree) : int =
    let rec longestZigZag' node depth goLeft =
        match node with
        | Leaf -> depth - 1
        | Node(_, left, right) ->
            if goLeft then
                Math.Max(longestZigZag' left (depth + 1) false, longestZigZag' right 1 true)
            else
                Math.Max(longestZigZag' right (depth + 1) true, longestZigZag' left 1 false)

    Math.Max(longestZigZag' root 0 true, longestZigZag' root 0 false)

let tree1 =
    Node(
        1,
        Leaf,
        Node(
            1,
            Node(1, Leaf, Leaf),
            Node(1, Node(1, Leaf, Node(1, Leaf, Node(1, Leaf, Node(1, Leaf, Leaf)))), Node(1, Leaf, Leaf))
        )
    )
// 3
longestZigZa tree1

let tree2 =
    Node(1, Node(1, Leaf, Node(1, Node(1, Leaf, Node(1, Leaf, Leaf)), Node(1, Leaf, Leaf))), Node(1, Leaf, Leaf))
// 4
longestZigZa tree2

// 0
longestZigZa (Node(1, Leaf, Leaf))
