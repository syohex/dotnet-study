type Tree =
    | Leaf
    | Node of int * Tree * Tree

let lowestCommonAncestor (root: Tree) (p: int) (q: int) : int =
    let rec collectAncestors node target acc =
        match node with
        | Leaf -> [], false
        | Node (v, left, right) ->
            let acc' = v :: acc

            if v = target then
                acc' |> List.rev, true
            else
                match collectAncestors left target acc' with
                | acc'', true -> acc'', true
                | _, _ -> collectAncestors right target acc'

    let rec lowestCommonAncestor' ps qs prev =
        match ps, qs with
        | h1 :: t1, h2 :: t2 ->
            if h1 = h2 then
                lowestCommonAncestor' t1 t2 h1
            else
                prev
        | _, _ -> prev

    let pAncestors, _ = collectAncestors root p []
    let qAncestors, _ = collectAncestors root q []

    lowestCommonAncestor' pAncestors qAncestors -1

let tree1 =
    Node(
        3,
        Node(5, Node(6, Leaf, Leaf), Node(2, Node(7, Leaf, Leaf), Node(4, Leaf, Leaf))),
        Node(1, Node(0, Leaf, Leaf), Node(8, Leaf, Leaf))
    )
// 3
lowestCommonAncestor tree1 5 1

// 5
lowestCommonAncestor tree1 5 4

let tree2 = Node(1, Node(2, Leaf, Leaf), Leaf)
// 1
lowestCommonAncestor tree2 1 2
