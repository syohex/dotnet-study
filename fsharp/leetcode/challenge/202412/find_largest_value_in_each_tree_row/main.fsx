type Tree =
    | Leaf
    | Node of int * Tree * Tree

let largestValues (root: Tree) : int list =
    let rec largestValues' q acc =
        match q with
        | [] -> List.rev acc
        | _ ->
            let q, maxVal =
                q
                |> List.fold
                    (fun (q, maxVal) node ->
                        match node with
                        | Leaf -> q, maxVal
                        | Node(v, Leaf, Leaf) -> q, max maxVal v
                        | Node(v, Leaf, right) -> right :: q, max maxVal v
                        | Node(v, left, Leaf) -> left :: q, max maxVal v
                        | Node(v, left, right) -> right :: left :: q, max maxVal v)
                    ([], System.Int32.MinValue)

            largestValues' q (maxVal :: acc)

    match root with
    | Leaf -> []
    | _ -> largestValues' [ root ] []

let tree1 =
    Node(1, Node(3, Node(5, Leaf, Leaf), Node(3, Leaf, Leaf)), Node(2, Leaf, Node(9, Leaf, Leaf)))
// [1,3,9]
largestValues tree1

let tree2 = Node(1, Node(2, Leaf, Leaf), Node(3, Leaf, Leaf))
// [1,3]
largestValues tree2
