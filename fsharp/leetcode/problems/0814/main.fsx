type Tree =
    | Leaf
    | Node of int * Tree * Tree

let pruneTree (root: Tree) : Tree =
    let rec pruneTree' node =
        match node with
        | Leaf -> Leaf, true
        | Node (v, left, right) ->
            let left', ret1 = pruneTree' left
            let right', ret2 = pruneTree' right

            let left'' = if ret1 then Leaf else left'
            let right'' = if ret2 then Leaf else right'

            Node(v, left'', right''), ret1 && ret2 && v = 0


    let tree, ret = pruneTree' root
    if ret then Leaf else tree

let tree1 = Node(1, Leaf, Node(0, Node(0, Leaf, Leaf), Node(1, Leaf, Leaf)))
// [1, null, 0, null, 1]
pruneTree tree1

let tree2 =
    Node(1, Node(0, Node(0, Leaf, Leaf), Node(0, Leaf, Leaf)), Node(1, Node(0, Leaf, Leaf), Node(1, Leaf, Leaf)))
// [1,null,1,null,1]
pruneTree tree2

let tree3 =
    Node(
        1,
        Node(1, Node(1, Node(0, Leaf, Leaf), Leaf), Node(1, Leaf, Leaf)),
        Node(0, Node(0, Leaf, Leaf), Node(1, Leaf, Leaf))
    )
// [1,1,0,1,1,null,1]
pruneTree tree3
