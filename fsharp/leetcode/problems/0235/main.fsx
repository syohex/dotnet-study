type Tree =
    | Leaf
    | Node of int * Tree * Tree

let lowestCommonAncestor (root: Tree) (p: int) (q: int) : int =
    let rec collectAncestors' node num acc =
        match node with
        | Leaf -> None
        | Node (v, left, right) ->
            let acc' = v :: acc

            if v = num then
                Some(acc' |> List.rev)
            else
                match collectAncestors' left num acc' with
                | None -> collectAncestors' right num acc'
                | ret -> ret

    let rec lowestCommonAncestor' ps qs prev =
        match ps, qs with
        | [], []
        | [], _
        | _, [] -> prev
        | h1 :: t1, h2 :: t2 ->
            if h1 = h2 then
                lowestCommonAncestor' t1 t2 h1
            else
                prev


    let pAncestors = (collectAncestors' root p []).Value
    let qAncestors = (collectAncestors' root q []).Value

    lowestCommonAncestor' pAncestors qAncestors System.Int32.MinValue


let tree1 =
    Node(
        6,
        Node(2, Node(0, Leaf, Leaf), Node(4, Node(3, Leaf, Leaf), Node(5, Leaf, Leaf))),
        Node(8, Node(7, Leaf, Leaf), Node(9, Leaf, Leaf))
    )
// 6
lowestCommonAncestor tree1 2 8

// 2
lowestCommonAncestor tree1 2 4

let tree2 =
    Node(
        6,
        Node(2, Node(0, Leaf, Leaf), Node(4, Node(3, Leaf, Leaf), Node(5, Leaf, Leaf))),
        Node(8, Node(7, Leaf, Leaf), Node(9, Leaf, Leaf))
    )
// 4
lowestCommonAncestor tree2 3 5
