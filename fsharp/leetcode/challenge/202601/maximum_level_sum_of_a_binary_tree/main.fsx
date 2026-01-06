type TreeNode =
    | Leaf
    | Node of int * TreeNode * TreeNode

let maxLevelSum (root: TreeNode) : int =
    let rec maxLevelSum' (nodes: TreeNode list) level maxSum maxLevel =
        match nodes with
        | [] -> maxLevel
        | _ ->
            let sum =
                nodes
                |> List.map (fun n ->
                    match n with
                    | Leaf -> 0
                    | Node(v, _, _) -> v)
                |> List.sum

            let nodes =
                nodes
                |> List.fold
                    (fun acc n ->
                        match n with
                        | Leaf -> acc
                        | Node(_, left, right) -> right :: left :: acc)
                    []
                |> List.filter ((<>) Leaf)

            let maxSum, maxLevel = if sum > maxSum then sum, level else maxSum, maxLevel
            maxLevelSum' nodes (level + 1) maxSum maxLevel

    maxLevelSum' [ root ] 1 System.Int32.MinValue 1

let tree1 =
    Node(1, Node(7, Node(7, Leaf, Leaf), Node(-8, Leaf, Leaf)), Node(0, Leaf, Leaf))
// 2
maxLevelSum tree1

let tree2 =
    Node(989, Leaf, Node(10250, Node(98693, Leaf, Leaf), Node(-89388, Leaf, Node(-32127, Leaf, Leaf))))
// 2
maxLevelSum tree2
