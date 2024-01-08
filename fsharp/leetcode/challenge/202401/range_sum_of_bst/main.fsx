type Tree =
    | Leaf
    | Node of int * Tree * Tree

let rangeSumBST (root: Tree) (low: int) (high: int) : int =
    let rec rangeSumBST' node low high =
        match node with
        | Leaf -> 0
        | Node(v, left, right) ->
            let sumLeft = rangeSumBST' left low high
            let sumRight = rangeSumBST' right low high

            if v >= low && v <= high then
                v + sumLeft + sumRight
            else
                sumLeft + sumRight

    rangeSumBST' root low high

let tree1 =
    Node(10, Node(5, Node(3, Leaf, Leaf), Node(7, Leaf, Leaf)), Node(15, Leaf, Node(18, Leaf, Leaf)))
// 32
rangeSumBST tree1 7 15

let tree2 =
    Node(
        10,
        Node(5, Node(3, Node(1, Leaf, Leaf), Leaf), Node(7, Node(6, Leaf, Leaf), Leaf)),
        Node(15, Node(13, Leaf, Leaf), Node(18, Leaf, Leaf))
    )
// 23
rangeSumBST tree2 6 10
