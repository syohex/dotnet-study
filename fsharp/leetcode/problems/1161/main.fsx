open System

type Tree =
    | Leaf
    | Node of int * Tree * Tree

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

let tree1 =
    Node(1, Node(7, Node(7, Leaf, Leaf), Node(-8, Leaf, Leaf)), Node(0, Leaf, Leaf))
// 2
maxLevelSum tree1

let tree2 =
    Node(989, Leaf, Node(10250, Node(98693, Leaf, Leaf), Node(-89388, Leaf, Node(-32127, Leaf, Leaf))))
// 2
maxLevelSum tree2
