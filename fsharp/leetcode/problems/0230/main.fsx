type Tree =
    | Leaf
    | Node of int * Tree * Tree

let kthSmallest (root: Tree) (k: int) : int =
    let rec kthSmallest' node acc =
        match node with
        | Leaf -> acc
        | Node (v, left, right) ->
            let acc' = kthSmallest' left acc
            kthSmallest' right  (v :: acc')

    kthSmallest' root []
    |> List.rev
    |> List.item (k - 1)


let tree1 = Node(3, Node(1, Leaf, Node(2, Leaf, Leaf)), Node(4, Leaf, Leaf))
// 1
kthSmallest tree1 1

let tree2 =
    Node(5, Node(3, Node(2, Node(1, Leaf, Leaf), Leaf), Node(4, Leaf, Leaf)), Node(6, Leaf, Leaf))

// 3
kthSmallest tree2 3
