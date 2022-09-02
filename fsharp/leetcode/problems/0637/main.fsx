type Tree =
    | Leaf
    | Node of int * Tree * Tree

let averageOfLevels (root: Tree) : double list =
    let rec averageOfLevels' node level acc =
        match node with
        | Leaf -> acc
        | Node (v, left, right) ->
            let acc' =
                match Map.tryFind level acc with
                | None -> Map.add level (v, 1) acc
                | Some ((sum, count)) -> Map.add level (sum + v, count + 1) acc

            let acc'' = averageOfLevels' left (level + 1) acc'
            averageOfLevels' right (level + 1) acc''

    let m = averageOfLevels' root 0 Map.empty

    let max =
        m
        |> Map.fold (fun acc k _ -> System.Math.Max(acc, k)) 0

    seq { 0 .. max }
    |> Seq.map (fun i ->
        let sum, count = Map.find i m
        (double sum) / (double count))
    |> Seq.toList

let tree1 =
    Node(3, Node(9, Leaf, Leaf), Node(20, Node(15, Leaf, Leaf), Node(7, Leaf, Leaf)))
// [3.00000,14.50000,11.00000]
averageOfLevels tree1

let tree2 =
    Node(3, Node(9, Node(15, Leaf, Leaf), Node(7, Leaf, Leaf)), Node(20, Leaf, Leaf))
// [3.00000,14.50000,11.00000]
averageOfLevels tree2
