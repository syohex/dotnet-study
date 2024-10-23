type Tree =
    | Leaf
    | Node of int * Tree * Tree

let sibilingSum (root: Tree) : Map<int, int> =
    let rec sibilingSum' q level acc =
        match q with
        | [] -> acc
        | _ ->
            let sum =
                q
                |> List.fold
                    (fun acc n ->
                        match n with
                        | Leaf -> failwith "never reach here"
                        | Node(v, _, _) -> acc + v)
                    0

            let acc = Map.add level sum acc

            let q =
                q
                |> List.fold
                    (fun acc n ->
                        match n with
                        | Leaf -> acc
                        | Node(_, left, right) -> right :: left :: acc)
                    []
                |> List.filter (fun n -> n <> Leaf)

            sibilingSum' q (level + 1) acc

    sibilingSum' [ root ] 0 Map.empty

let replaceValueInTree (root: Tree) : Tree =
    let rec replaceValueInTree' node nonCousinSum level sumMap =
        match node with
        | Leaf -> Leaf
        | Node(_, left, right) ->
            let s =
                match left with
                | Leaf -> 0
                | Node(v, _, _) -> v

            let s =
                match right with
                | Leaf -> s
                | Node(v, _, _) -> s + v

            let sum = Map.find level sumMap

            Node(
                sum - nonCousinSum,
                replaceValueInTree' left s (level + 1) sumMap,
                replaceValueInTree' right s (level + 1) sumMap
            )

    let sumMap = sibilingSum root

    match root with
    | Leaf -> failwith "never reach here"
    | Node(v, _, _) -> replaceValueInTree' root v 0 sumMap

let tree1 =
    Node(5, Node(4, Node(1, Leaf, Leaf), Node(10, Leaf, Leaf)), Node(9, Leaf, Node(7, Leaf, Leaf)))
// [0,0,0,7,7,null,11]
replaceValueInTree tree1

let tree2 = Node(3, Node(1, Leaf, Leaf), Node(2, Leaf, Leaf))
// [0,0,0]
replaceValueInTree tree2
