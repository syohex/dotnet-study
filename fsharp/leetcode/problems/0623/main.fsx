type Tree =
    | Leaf
    | Node of int * Tree * Tree


let addOneRow (root: Tree) (value: int) (depth: int) : Tree =
    let rec addOneRow' node d value depth =
        match node with
        | Leaf -> Leaf
        | Node (v, left, right) ->
            if d + 1 = depth then
                let newLeft = Node(value, left, Leaf)
                let newRight = Node(value, Leaf, right)
                Node(v, newLeft, newRight)
            else
                Node(v, addOneRow' left (d + 1) value depth, addOneRow' right (d + 1) value depth)

    if depth = 1 then
        Node(value, root, Leaf)
    else
        addOneRow' root 1 value depth

let tree1 =
    Node(4, Node(2, Node(3, Leaf, Leaf), Node(1, Leaf, Leaf)), Node(6, Node(5, Leaf, Leaf), Leaf))
// [4,1,1,2,null,null,6,3,1,5]
addOneRow tree1 1 2

let tree2 = Node(4, Node(2, Node(3, Leaf, Leaf), Node(1, Leaf, Leaf)), Leaf)
// [4,2,null,1,1,3,null,null,1]
addOneRow tree2 1 3

let tree3 = Node(1, Node(2, Node(4, Leaf, Leaf), Leaf), Node(3, Leaf, Leaf))
// [1,2,3,4,null,null,null,5,5]
addOneRow tree3 5 4

let tree4 =
    Node(4, Node(2, Node(3, Leaf, Leaf), Node(1, Leaf, Leaf)), Node(6, Node(5, Leaf, Leaf), Leaf))
// [1,4,null,2,6,3,1,5]
addOneRow tree4 1 1
