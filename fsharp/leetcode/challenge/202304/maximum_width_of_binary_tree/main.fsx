type Tree =
    | Leaf
    | Node of int * Tree * Tree

let widthOfBinaryTree (root: Tree) : int =
    let rec widthOfBinaryTree' q ret =
        match q with
        | [] -> ret
        | (_, baseValue) :: _ ->
            let lastValue = List.last q |> snd

            let q' =
                q
                |> List.fold
                    (fun q (node, value) ->
                        match node with
                        | Leaf -> q
                        | Node(_, left, right) ->
                            match left, right with
                            | Leaf, Leaf -> q
                            | Node(_), Leaf ->
                                (left, value * 2 - baseValue) :: q
                            | Leaf, Node(_) ->
                                (right, value * 2 + 1 - baseValue) :: q
                            | Node(_), Node(_) ->
                                let leftValue = (value * 2) - baseValue
                                let rightValue = (value * 2 + 1) - baseValue
                                (right, rightValue) :: (left, leftValue) :: q)
                    [] |> List.rev

            let ret' = System.Math.Max(ret, lastValue - baseValue + 1)
            widthOfBinaryTree' q' ret'

    widthOfBinaryTree' [ (root, 0) ] 0

let tree1 =
    Node(1, Node(3, Node(5, Leaf, Leaf), Node(3, Leaf, Leaf)), Node(2, Leaf, Node(9, Leaf, Leaf)))
// 4
widthOfBinaryTree tree1

let tree2 =
    Node(1, Node(3, Node(5, Node(6, Leaf, Leaf), Leaf), Leaf), Node(2, Leaf, Node(9, Node(7, Leaf, Leaf), Leaf)))
// 7
widthOfBinaryTree tree2

let tree3 = Node(1, Node(3, Node(5, Leaf, Leaf), Leaf), Node(2, Leaf, Leaf))
// 2
widthOfBinaryTree tree3
