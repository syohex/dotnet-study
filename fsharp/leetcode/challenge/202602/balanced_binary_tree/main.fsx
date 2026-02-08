type Tree =
    | Leaf
    | Node of int * Tree * Tree

let isBalanced (root: Tree) : bool =
    let rec isBalanced' node depth =
        match node with
        | Leaf -> depth, true
        | Node(_, left, right) ->
            let depth1, ok1 = isBalanced' left (depth + 1)
            let depth2, ok2 = isBalanced' right (depth + 1)

            if not ok1 || not ok2 then
                depth, false
            else
                let diff = abs (depth1 - depth2)
                if diff > 1 then depth, false else max depth1 depth2, true

    isBalanced' root 0 |> snd

let tree1 =
    Node(3, Node(9, Leaf, Leaf), Node(20, Node(5, Leaf, Leaf), Node(7, Leaf, Leaf)))
// true
isBalanced tree1

let tree2 =
    Node(1, Node(2, Node(3, Node(4, Leaf, Leaf), Node(4, Leaf, Leaf)), Node(3, Leaf, Leaf)), Node(2, Leaf, Leaf))
// false
isBalanced tree2

// true
isBalanced Leaf

let tree4 = Node(1, Leaf, Node(2, Leaf, Node(3, Leaf, Leaf)))
// false
isBalanced tree4

let tree5 =
    Node(1, Node(2, Node(4, Node(8, Leaf, Leaf), Leaf), Node(5, Leaf, Leaf)), Node(3, Node(6, Leaf, Leaf), Leaf))
// true
isBalanced tree5
