type Tree =
    | Leaf
    | Node of int * Tree * Tree

let toValue node =
    match node with
    | Leaf -> failwith "never reach here"
    | Node(v, _, _) -> v

let valsToTree (vals: int[] list) : Tree =
    let rec valsToTree' (vals: int[] list) len =
        match vals with
        | [] -> Array.init len (fun _ -> Leaf)
        | parents :: t ->
            let children = valsToTree' t (len * 2)

            parents
            |> Array.indexed
            |> Array.map (fun (i, v) -> Node(v, children.[2 * i], children.[2 * i + 1]))

    valsToTree' vals 1 |> Array.item 0

let reverseOddLevels (root: Tree) : Tree =
    let rec reverseOddLevels' q level =
        match q with
        | [] -> []
        | _ ->
            let vals = q |> List.map toValue |> List.toArray
            let vals = if level % 2 = 1 then Array.rev vals else vals

            let children =
                q
                |> List.fold
                    (fun acc node ->
                        match node with
                        | Leaf -> acc
                        | Node(_, Leaf, Leaf) -> acc
                        | Node(_, left, right) -> right :: left :: acc)
                    []
                |> List.rev

            vals :: reverseOddLevels' children (level + 1)

    let vals = reverseOddLevels' [ root ] 0
    valsToTree vals

let tree1 =
    Node(2, Node(3, Node(8, Leaf, Leaf), Node(13, Leaf, Leaf)), Node(5, Node(21, Leaf, Leaf), Node(34, Leaf, Leaf)))

// [2,5,3,8,13,21,34]
reverseOddLevels tree1

let tree2 = Node(7, Node(13, Leaf, Leaf), Node(11, Leaf, Leaf))
// [7,11,13]
reverseOddLevels tree2

let tree3 =
    Node(
        0,
        Node(1, Node(0, Node(1, Leaf, Leaf), Node(1, Leaf, Leaf)), Node(0, Node(1, Leaf, Leaf), Node(1, Leaf, Leaf))),
        Node(2, Node(0, Node(2, Leaf, Leaf), Node(2, Leaf, Leaf)), Node(0, Node(2, Leaf, Leaf), Node(2, Leaf, Leaf)))
    )

reverseOddLevels tree3
