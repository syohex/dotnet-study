type TreeNode =
    | EOT
    | Node of int * TreeNode * TreeNode

type NodePos = { Node: TreeNode; Pos: int }

let childNodePos (node: NodePos) (basePos: int) : NodePos list =
    match node.Node with
    | EOT -> []
    | Node (_, left, right) ->
        let leftChild =
            match left with
            | EOT -> []
            | Node (_, _, _) ->
                [ { Node = left
                    Pos = 2 * node.Pos - basePos } ]

        let rightChild =
            match right with
            | EOT -> []
            | Node (_, _, _) ->
                [ { Node = right
                    Pos = 2 * node.Pos + 1 - basePos } ]

        rightChild @ leftChild

let widthOfBinaryTree (root: TreeNode) : int =
    let rec widthOfBinaryTree' (ns: NodePos list) ret =
        match ns with
        | [] -> ret + 1
        | head :: tail ->
            let newRet =
                tail
                |> List.fold
                    (fun acc np ->
                        let diff = np.Pos - head.Pos
                        if diff > acc then diff else diff)
                    ret

            let newCandidates =
                ns
                |> List.fold (fun acc np -> (childNodePos np head.Pos) @ acc) []
                |> List.rev

            widthOfBinaryTree' newCandidates newRet

    widthOfBinaryTree' [ { Node = root; Pos = 0 } ] 0

// 4
let tree1 =
    Node(1, Node(3, Node(5, EOT, EOT), Node(3, EOT, EOT)), Node(2, EOT, Node(9, EOT, EOT)))

widthOfBinaryTree tree1

// 2
let tree2 = Node(1, Node(3, Node(5, EOT, EOT), Node(3, EOT, EOT)), EOT)

widthOfBinaryTree tree2

// 2
let tree3 = Node(1, Node(3, Node(5, EOT, EOT), EOT), Node(2, EOT, EOT))

widthOfBinaryTree tree3
