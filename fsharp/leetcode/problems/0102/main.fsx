type Tree =
    | Leaf
    | Node of int * Tree * Tree

    static member value(t: Tree) =
        match t with
        | Leaf -> failwith "never reach here"
        | Node (v, _, _) -> v

    static member isNode(t: Tree) =
        match t with
        | Node (_) -> true
        | Leaf -> false

let levelOrder (root: Tree) : int list list =
    let rec levelOrder' nodes acc =
        match nodes with
        | [] -> acc |> List.rev
        | _ ->
            let vals =
                nodes
                |> List.fold (fun acc n -> (Tree.value n) :: acc) []
                |> List.rev

            let nodes' =
                nodes
                |> List.fold
                    (fun acc n ->
                        match n with
                        | Node (_, left, right) -> right :: left :: acc
                        | Leaf -> failwith "never reach here")
                    []
                |> List.filter Tree.isNode
                |> List.rev

            levelOrder' nodes' (vals :: acc)

    match root with
    | Leaf -> []
    | _ -> levelOrder' [ root ] []

let tree1 =
    Node(3, Node(9, Leaf, Leaf), Node(20, Node(15, Leaf, Leaf), Node(7, Leaf, Leaf)))
// [[3],[9,20],[15,7]]
levelOrder tree1

let tree2 = Node(1, Leaf, Leaf)
// [[1]]
levelOrder tree2

// []
levelOrder Leaf
