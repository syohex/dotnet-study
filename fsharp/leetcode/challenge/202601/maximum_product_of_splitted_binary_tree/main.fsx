type TreeNode =
    | Leaf
    | Node of int * TreeNode * TreeNode

let maxProduct (root: TreeNode) : int =
    let rec collectSums node acc =
        match node with
        | Leaf -> 0, acc
        | Node(v, left, right) ->
            let v1, acc = collectSums left acc
            let v2, acc = collectSums right acc
            let v = v + v1 + v2
            v, Set.add v acc

    let sum, sums = collectSums root Set.empty

    sums
    |> Set.fold
        (fun ret v ->
            let diff = sum - v
            max ret (int64 v * int64 diff))
        0L
    |> fun n -> n % 1_000_000_007L |> int

let tree1 =
    Node(1, Node(2, Node(4, Leaf, Leaf), Node(5, Leaf, Leaf)), Node(3, Node(6, Leaf, Leaf), Leaf))
// 110
maxProduct tree1

let tree2 =
    Node(1, Leaf, Node(2, Node(3, Leaf, Leaf), Node(4, Node(5, Leaf, Leaf), Node(6, Leaf, Leaf))))
// 90
maxProduct tree2
