open System

type TreeNode =
    | Leaf
    | Node of int * TreeNode * TreeNode

let tree2Str (root: TreeNode) : string =
    let rec tree2Str' (node: TreeNode) (acc: string list) =
        match node with
        | Leaf -> acc
        | Node(v, left, right) ->
            let acc' = (string v) :: acc

            match left, right with
            | Leaf, Leaf -> acc'
            | Node(_), Leaf ->
                let acc'' = tree2Str' left ("(" :: acc')
                ")" :: acc''
            | Leaf, Node(_) ->
                let acc' = tree2Str' right ("(" :: ")" :: "(" :: acc')
                ")" :: acc'
            | _ ->
                let acc'' = tree2Str' left ("(" :: acc')
                let acc''' = tree2Str' right ("(" :: ")" :: acc'')
                ")" :: acc'''

    tree2Str' root [] |> List.rev |> String.Concat

let tree1 = Node(1, Node(2, Node(4, Leaf, Leaf), Leaf), Node(3, Leaf, Leaf))
// "1(2(4))(3)"
tree2Str tree1

let tree2 = Node(1, Node(2, Leaf, Node(4, Leaf, Leaf)), Node(3, Leaf, Leaf))
// "1(2()(4))(3)"
tree2Str tree2
