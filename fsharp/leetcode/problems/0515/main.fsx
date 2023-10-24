open System

type TreeNode =
    | Leaf
    | Node of int * TreeNode * TreeNode

let largestValues (root: TreeNode) : int list =
    let rec largestValues' q acc =
        match q with
        | [] -> List.rev acc
        | _ ->
            let q', max =
                q
                |> List.fold
                    (fun (q, max) node ->
                        match node with
                        | Leaf -> failwith "never reach here"
                        | Node(v, left, right) ->
                            let max' = Math.Max(max, v)

                            match left, right with
                            | Leaf, Leaf -> q, max'
                            | Leaf, Node(_) -> right :: q, max'
                            | Node(_), Leaf -> left :: q, max'
                            | _, _ -> left :: right :: q, max')
                    ([], Int32.MinValue)

            largestValues' q' (max :: acc)

    match root with
    | Leaf -> []
    | _ -> largestValues' [ root ] []

let tree1 =
    Node(1, Node(3, Node(5, Leaf, Leaf), Node(3, Leaf, Leaf)), Node(2, Leaf, Node(9, Leaf, Leaf)))
// [1;3;9]
largestValues tree1

let tree2 = Node(1, Node(2, Leaf, Leaf), Node(3, Leaf, Leaf))
largestValues tree2
