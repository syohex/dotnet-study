open System

type Tree =
    | Leaf
    | Node of int * Tree * Tree

let findMode (root: Tree) : int list =
    let rec nodeFreq node acc =
        match node with
        | Leaf -> acc
        | Node(v, left, right) ->
            let acc' =
                match Map.tryFind v acc with
                | Some(n) -> Map.add v (n + 1) acc
                | None -> Map.add v 1 acc

            let acc'' = nodeFreq left acc'
            nodeFreq right acc''

    let freq = nodeFreq root Map.empty
    let mode = Map.fold (fun acc _ (v: int) -> Math.Max(acc, v)) -1 freq
    freq |> Map.fold (fun acc k v -> if v = mode then k :: acc else acc) []

let tree1 = Node(1, Leaf, Node(2, Leaf, Node(2, Leaf, Leaf)))
// [2]
findMode tree1

let tree2 = Node(0, Leaf, Leaf)
// [0]
findMode tree2
