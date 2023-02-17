type Tree =
    | Leaf
    | Node of int * Tree * Tree

let minDiffInBST (root: Tree) : int =
    let rec collectValues node acc =
        match node with
        | Leaf -> acc
        | Node(v, left, right) ->
            let acc' = collectValues left (v :: acc)
            collectValues right acc'

    collectValues root []
    |> List.sort
    |> List.pairwise
    |> List.fold (fun acc (v1, v2) -> System.Math.Min(acc, v2 - v1)) System.Int32.MaxValue

let tree1 =
    Node(4, Node(2, Node(1, Leaf, Leaf), Node(3, Leaf, Leaf)), Node(6, Leaf, Leaf))
// 1
minDiffInBST tree1

let tree2 =
    Node(1, Node(0, Leaf, Leaf), Node(48, Node(12, Leaf, Leaf), Node(49, Leaf, Leaf)))
// 1
minDiffInBST tree2
