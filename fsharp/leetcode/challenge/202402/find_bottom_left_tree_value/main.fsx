type Tree =
    | Leaf
    | Node of int * Tree * Tree

let findLeftBottom (root: Tree) : int =
    let rec findLeftBottom' q acc =
        match q with
        | [] -> acc
        | Leaf :: _ -> failwith "never reach here"
        | Node(v, _, _) :: _ ->
            let q' =
                q
                |> List.fold
                    (fun acc node ->
                        match node with
                        | Leaf -> acc
                        | Node(_, left, right) -> right :: left :: acc)
                    []
                |> List.filter (fun node -> node <> Leaf)
                |> List.rev

            findLeftBottom' q' v

    findLeftBottom' [ root ] 0

let tree1 = Node(2, Node(1, Leaf, Leaf), Node(3, Leaf, Leaf))
// 1
findLeftBottom tree1

let tree2 =
    Node(1, Node(2, Node(4, Leaf, Leaf), Leaf), Node(3, Node(5, Node(7, Leaf, Leaf), Leaf), Node(6, Leaf, Leaf)))
// 7
findLeftBottom tree2
