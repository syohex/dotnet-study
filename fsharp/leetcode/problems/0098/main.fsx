open System

type Tree =
    | Leaf
    | Node of int64 * Tree * Tree

let isValidBST (root: Tree) : bool =
    let rec isValidBST' node (min: int64) (max: int64) =
        match node with
        | Leaf -> true
        | Node (v, left, right) ->
            if v <= min || v >= max then
                false
            else
                isValidBST' left min v && isValidBST' right v max

    isValidBST' root Int32.MinValue Int32.MaxValue

let tree1 = Node(2, Node(1, Leaf, Leaf), Node(3, Leaf, Leaf))
// true
isValidBST tree1

let tree2 =
    Node(5, Node(1, Leaf, Leaf), Node(4, Node(3, Leaf, Leaf), Node(6, Leaf, Leaf)))
// false
isValidBST tree2

let tree3 = Node(1, Node(1, Leaf, Leaf), Leaf)
// false
isValidBST tree3

let tree4 =
    Node(10, Node(5, Leaf, Leaf), Node(15, Node(6, Leaf, Leaf), Node(20, Leaf, Leaf)))
// false
isValidBST tree4

let tree5 =
    Node(3, Node(1, Node(0, Leaf, Leaf), Node(2, Leaf, Leaf)), Node(5, Node(4, Leaf, Leaf), Node(6, Leaf, Leaf)))
// true
isValidBST tree5
