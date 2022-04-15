type Tree =
    | Leaf
    | Node of int * Tree * Tree

let rec trimBST (root: Tree) (low: int) (high: int) =
    match root with
    | Leaf -> Leaf
    | Node (v, left, right) ->
        let left' = trimBST left low high
        let right' = trimBST right low high

        if v >= low && v <= high then
            Node(v, left', right')
        else
            match left' with
            | Leaf -> right'
            | _ -> left'


let tree1 =
    Node(1, Node(0, Leaf, Leaf), Node(2, Leaf, Leaf))
// [1, null, 2]
trimBST tree1 1 2

let tree2 =
    Node(3, Node(0, Leaf, Node(2, Node(1, Leaf, Leaf), Leaf)), Node(4, Leaf, Leaf))
// [3,2,null,1]
trimBST tree2 1 3
