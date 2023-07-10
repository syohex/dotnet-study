type Tree =
    | Leaf
    | Node of int * Tree * Tree

let minDepth (root: Tree) : int =
    let rec minDepth' q depth =
        match q with
        | [] -> depth
        | _ ->
            let q', ok =
                q
                |> List.fold
                    (fun (acc, ok) n ->
                        if ok then
                            acc, ok
                        else
                            match n with
                            | Leaf -> failwith "never reach here"
                            | Node(_, left, right) ->
                                match left, right with
                                | Leaf, Leaf -> acc, true
                                | Node(_), Leaf -> left :: acc, false
                                | Leaf, Node(_) -> right :: acc, false
                                | _ -> right :: left :: acc, false)
                    ([], false)

            if ok then depth else minDepth' q' (depth + 1)

    match root with
    | Leaf -> 0
    | _ -> minDepth' [ root ] 1

let tree1 =
    Node(3, Node(9, Leaf, Leaf), Node(20, Node(15, Leaf, Leaf), Node(7, Leaf, Leaf)))
// 2
minDepth tree1

let tree2 =
    Node(2, Leaf, Node(3, Leaf, Node(4, Leaf, Node(5, Leaf, Node(6, Leaf, Leaf)))))
// 5
minDepth tree2

// 0
minDepth Leaf

// 1
minDepth (Node(1, Leaf, Leaf))
