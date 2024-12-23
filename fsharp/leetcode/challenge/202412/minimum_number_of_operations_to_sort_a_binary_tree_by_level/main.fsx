type Tree =
    | Leaf
    | Node of int * Tree * Tree

let minimumOperations (root: Tree) : int =
    let countSwaps orig =
        let rec countSwaps' sorted i (orig: int[]) origMap acc =
            match sorted with
            | [] -> acc
            | h :: t ->
                if h = orig.[i] then
                    countSwaps' t (i + 1) orig origMap acc
                else
                    let origIndex = Map.find h origMap
                    let origMap = Map.add orig.[i] origIndex origMap
                    let tmp = orig.[i]
                    orig.[i] <- orig.[origIndex]
                    orig.[origIndex] <- tmp
                    countSwaps' t (i + 1) orig origMap (acc + 1)

        let sorted = orig |> List.sort

        let origMap =
            orig |> List.indexed |> List.fold (fun acc (i, v) -> Map.add v i acc) Map.empty

        countSwaps' sorted 0 (List.toArray orig) origMap 0

    let rec minimumOperations' q acc =
        match q with
        | [] -> acc
        | _ ->
            let orig =
                q
                |> List.fold
                    (fun acc node ->
                        match node with
                        | Leaf -> acc
                        | Node(v, _, _) -> v :: acc)
                    []
                |> List.rev

            let q =
                q
                |> List.fold
                    (fun acc node ->
                        match node with
                        | Leaf -> acc
                        | Node(_, left, Leaf) -> left :: acc
                        | Node(_, Leaf, right) -> right :: acc
                        | Node(v, left, right) -> right :: left :: acc)
                    []
                |> List.rev

            let ops = countSwaps orig
            minimumOperations' q (acc + ops)

    minimumOperations' [ root ] 0

let tree1 =
    Node(
        1,
        Node(4, Node(7, Leaf, Leaf), Node(6, Leaf, Leaf)),
        Node(3, Node(8, Node(9, Leaf, Leaf), Leaf), Node(5, Node(10, Leaf, Leaf), Leaf))
    )
// 3
minimumOperations tree1

let tree2 =
    Node(1, Node(3, Node(7, Leaf, Leaf), Node(6, Leaf, Leaf)), Node(2, Node(5, Leaf, Leaf), Node(4, Leaf, Leaf)))
// 3
minimumOperations tree2

let tree3 =
    Node(1, Node(2, Node(4, Leaf, Leaf), Node(5, Leaf, Leaf)), Node(3, Node(6, Leaf, Leaf), Leaf))
// 0
minimumOperations tree3
