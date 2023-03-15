type Tree =
    | Leaf
    | Node of int * Tree * Tree

let isCompleteTree (root: Tree) : bool =
    let rec countNodes node =
        match node with
        | Leaf -> 0
        | Node(_, left, right) -> 1 + (countNodes left) + (countNodes right)

    let rec isCompleteTree' q nodes =
        match q with
        | [] -> true
        | _ ->
            let valid =
                q
                |> List.forall (fun (node, index) ->
                    match node with
                    | Leaf -> true
                    | _ -> index <= nodes)

            if valid then
                let q' =
                    q
                    |> List.fold
                        (fun acc (node, index) ->
                            match node with
                            | Leaf -> acc
                            | Node(_, left, right) -> (left, 2 * index) :: (right, 2 * index + 1) :: acc)
                        []

                isCompleteTree' q' nodes
            else
                false

    isCompleteTree' [ (root, 1) ] (countNodes root)

let tree1 =
    Node(1, Node(2, Node(4, Leaf, Leaf), Node(5, Leaf, Leaf)), Node(2, Node(6, Leaf, Leaf), Leaf))
// true
isCompleteTree tree1

let tree2 =
    Node(1, Node(2, Node(4, Leaf, Leaf), Node(5, Leaf, Leaf)), Node(2, Leaf, Node(7, Leaf, Leaf)))
// false
isCompleteTree tree2
