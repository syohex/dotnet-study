open System

type Tree =
    | Leaf
    | Node of int * Tree * Tree

// depth first
let maxLevelSum (root: Tree) : int =
    let rec maxLevelSum' node level acc =
        match node with
        | Leaf -> acc
        | Node(v, left, right) ->
            let acc' =
                match Map.tryFind level acc with
                | Some(n) -> Map.add level (n + v) acc
                | None -> Map.add level v acc

            let acc'' = maxLevelSum' left (level + 1) acc'
            maxLevelSum' right (level + 1) acc''

    maxLevelSum' root 1 Map.empty
    |> Map.fold
        (fun (max, ret) k v -> if v > max || (v = max && k < ret) then v, k else max, ret)
        (Int32.MinValue, Int32.MaxValue)
    |> snd

// breadth first
let maxLevelSum2 (root: Tree) : int =
    let rec maxLevelSum2' q level (max, ret) =
        let sum =
            q
            |> List.fold
                (fun acc node ->
                    match node with
                    | Leaf -> acc
                    | Node(v, _, _) -> acc + v)
                0

        let q' =
            q
            |> List.fold
                (fun acc node ->
                    match node with
                    | Leaf -> acc
                    | Node(_, left, right) -> left :: right :: acc)
                []

        match q' with
        | [] -> max, ret
        | _ ->
            if sum > max then
                maxLevelSum2' q' (level + 1) (sum, level)
            else
                maxLevelSum2' q' (level + 1) (max, ret)

    maxLevelSum2' [ root ] 1 (Int32.MinValue, 0) |> snd

let tree1 =
    Node(1, Node(7, Node(7, Leaf, Leaf), Node(-8, Leaf, Leaf)), Node(0, Leaf, Leaf))
// 2
maxLevelSum tree1
// 2
maxLevelSum2 tree1

let tree2 =
    Node(989, Leaf, Node(10250, Node(98693, Leaf, Leaf), Node(-89388, Leaf, Node(-32127, Leaf, Leaf))))
// 2
maxLevelSum tree2
// 2
maxLevelSum2 tree2
