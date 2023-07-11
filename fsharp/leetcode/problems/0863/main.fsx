type Tree =
    | Leaf
    | Node of int * Tree * Tree

let toGraph root =
    let rec toGraph' node acc =
        match node with
        | Leaf -> acc
        | Node(v, left, right) ->
            let acc' =
                match left with
                | Leaf -> acc
                | Node(v', _, _) ->
                    let tmp =
                        match Map.tryFind v acc with
                        | Some(nodes) -> Map.add v (v' :: nodes) acc
                        | None -> Map.add v [ v' ] acc

                    match Map.tryFind v' tmp with
                    | Some(nodes) -> Map.add v' (v :: nodes) tmp
                    | None -> Map.add v' [ v ] tmp

            let acc'' =
                match right with
                | Leaf -> acc'
                | Node(v', _, _) ->
                    let tmp =
                        match Map.tryFind v acc' with
                        | Some(nodes) -> Map.add v (v' :: nodes) acc'
                        | None -> Map.add v [ v' ] acc'

                    match Map.tryFind v' tmp with
                    | Some(nodes) -> Map.add v' (v :: nodes) tmp
                    | None -> Map.add v' [ v ] tmp

            toGraph' right (toGraph' left acc'')

    toGraph' root Map.empty

let distanceK (root: Tree) (target: int) (k: int) : int list =
    let rec distanceK' q graph k visited =
        if k = 0 then
            q
        else
            let q', visited' =
                q
                |> List.fold
                    (fun (acc, visited) n ->
                        match Map.tryFind n graph with
                        | None -> acc, visited
                        | Some(nodes) ->
                            let nodes' = nodes |> List.filter (fun node -> Set.contains node visited |> not)
                            let visited' = nodes' |> List.fold (fun acc n -> Set.add n acc) visited
                            nodes' @ acc, visited')
                    ([], visited)

            distanceK' q' graph (k - 1) visited'

    let graph = toGraph root
    let visited = Set.empty |> Set.add target
    distanceK' [ target ] graph k visited

let tree1 =
    Node(
        3,
        Node(5, Node(6, Leaf, Leaf), Node(2, Node(7, Leaf, Leaf), Node(4, Leaf, Leaf))),
        Node(1, Node(0, Leaf, Leaf), Node(8, Leaf, Leaf))
    )

// [7, 4, 1] in any order
distanceK tree1 5 2

let tree2 = Node(1, Leaf, Leaf)
// []
distanceK tree2 1 3
