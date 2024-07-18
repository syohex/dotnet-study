type Tree =
    | Leaf
    | Node of int * Tree * Tree

let collectLeaves (node: Tree) : Set<int> =
    let rec collectLeaves' node acc =
        match node with
        | Leaf -> acc
        | Node(v, Leaf, Leaf) -> Set.add v acc
        | Node(_, left, right) ->
            let acc = collectLeaves' left acc
            collectLeaves' right acc

    collectLeaves' node Set.empty

let treeToGraph (node: Tree) : Map<int, int list> =
    let rec treeToGraph' node prev acc =
        match node with
        | Leaf -> acc
        | Node(v, left, right) ->
            let acc =
                if prev = -1 then
                    acc
                else
                    match Map.tryFind prev acc with
                    | None -> Map.add prev [ v ] acc
                    | Some(w) -> Map.add prev (v :: w) acc

            let acc =
                if prev = -1 then
                    acc
                else
                    match Map.tryFind v acc with
                    | None -> Map.add v [ prev ] acc
                    | Some(w) -> Map.add v (prev :: w) acc

            let acc = treeToGraph' left v acc
            treeToGraph' right v acc

    treeToGraph' node -1 Map.empty

let countGoodLeaves (leaf: int) (distance: int) (leaves: Set<int>) (graph: Map<int, int list>) =
    let rec countGoodLeaves' q leaves graph i distance visited acc =
        if i > distance then
            acc
        else
            let visited = q |> List.fold (fun acc node -> Set.add node acc) visited

            let nexts =
                q
                |> List.fold (fun acc next -> if Set.contains next visited then next :: acc else acc) []

            let acc =
                nexts
                |> List.fold (fun acc node -> if Set.contains node leaves then acc + 1 else acc) 0

            countGoodLeaves' nexts leaves graph (i + 1) distance visited acc

    countGoodLeaves' [ leaf ] leaves graph 0 distance Set.empty 0

let countPairs (root: Tree) (distance: int) : int =
    let rec countPairs' nodes leaves graph distance acc =
        match nodes with
        | [] -> acc / 2
        | h :: t ->
            let ret = countGoodLeaves h distance leaves graph
            countPairs' t leaves graph distance (acc + ret)

    let leaves = collectLeaves root
    let graph = treeToGraph root
    countPairs' (Set.toList leaves) leaves graph distance 0

let tree1 = Node(1, Node(2, Leaf, Node(4, Leaf, Leaf)), Node(3, Leaf, Leaf))
countPairs tree1 3

let tree2 =
    Node(1, Node(2, Node(4, Leaf, Leaf), Node(5, Leaf, Leaf)), Node(3, Node(6, Leaf, Leaf), Node(7, Leaf, Leaf)))

countPairs tree2 3

let tree3 =
    Node(7, Node(1, Node(6, Leaf, Leaf), Leaf), Node(4, Node(5, Leaf, Leaf), Node(3, Leaf, Node(2, Leaf, Leaf))))

countPairs tree3 3
