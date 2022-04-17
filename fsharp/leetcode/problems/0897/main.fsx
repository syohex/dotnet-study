type Tree =
    | Leaf
    | Node of int * Tree * Tree

let rec appendRight (root: Tree) (node: Tree) : Tree =
    match root with
    | Leaf -> node
    | Node (v, _, right) -> Node(v, Leaf, (appendRight right node))

let rec increasingBST (root: Tree) : Tree =
    match root with
    | Leaf -> Leaf
    | Node (v, left, right) ->
        let ret1 = increasingBST left
        let ret2 = increasingBST right

        let ret = appendRight ret1 (Node(v, Leaf, Leaf))
        appendRight ret ret2

let tree1 =
    Node(
        5,
        Node(3, Node(2, Node(1, Leaf, Leaf), Leaf), Node(4, Leaf, Leaf)),
        Node(6, Leaf, Node(8, Node(7, Leaf, Leaf), Node(9, Leaf, Leaf)))
    )
//  [1,null,2,null,3,null,4,null,5,null,6,null,7,null,8,null,9]
increasingBST tree1

let tree2 = Node(5, Node(1, Leaf, Leaf), Node(7, Leaf, Leaf))
// [1, null, 5, null, 7]
increasingBST tree2
