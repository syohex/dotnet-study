type Tree =
    | Leaf
    | Node of int * Tree * Tree

let rightSideView (root: Tree) : int list =
    let rec rightSideView' q ret =
        match q with
        | [] -> ret |> List.rev
        | _ ->
            let q', v =
                q
                |> List.fold
                    (fun (acc, prev) n ->
                        match n with
                        | Leaf -> acc, prev
                        | Node (v, left, right) ->
                            match left, right with
                            | Leaf, Leaf -> acc, v
                            | Node (_), Leaf -> (left :: acc), v
                            | Leaf, Node (_) -> (right :: acc), v
                            | Node (_), Node (_) -> (right :: left :: acc), v)
                    ([], -1)

            rightSideView' (q' |> List.rev) (v :: ret)

    match root with
    | Leaf -> []
    | Node (_) -> rightSideView' [ root ] []

let tree1 =
    Node(1, Node(2, Leaf, Node(5, Leaf, Leaf)), Node(3, Leaf, Node(4, Leaf, Leaf)))
// [1;3;4]
rightSideView tree1

let tree2 = Node(1, Leaf, Node(3, Leaf, Leaf))
// [1;3]
rightSideView tree2

// []
rightSideView Leaf
