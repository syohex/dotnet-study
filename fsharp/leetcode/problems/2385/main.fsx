type Tree =
    | Leaf
    | Node of int * Tree * Tree

let toGraph (root: Tree) : Map<int, int list> =
    let defaultValue m k =
        Map.tryFind k m |> Option.defaultValue []

    let rec toGraph' node parent acc =
        match node with
        | Leaf -> acc
        | Node(v, left, right) ->
            let acc' =
                if parent <> -1 then
                    let acc' = Map.add parent (v :: (defaultValue acc parent)) acc
                    Map.add v (parent :: (defaultValue acc' v)) acc'
                else
                    acc

            let acc' = toGraph' left v acc'
            toGraph' right v acc'

    toGraph' root -1 Map.empty

let amountOfTime (root: Tree) (start: int) : int =
    let rec amountOfTime' q steps graph visited =
        match q with
        | [] -> steps
        | _ ->
            let visited' = q |> List.fold (fun acc n -> Set.add n acc) visited

            let q' =
                q
                |> List.fold
                    (fun q node ->
                        let q' =
                            match Map.tryFind node graph with
                            | None -> []
                            | Some(v) -> v |> List.filter (fun n -> not <| Set.contains n visited')

                        q' @ q)
                    []

            amountOfTime' q' (steps + 1) graph visited'

    let graph = toGraph root
    amountOfTime' [ start ] -1 graph Set.empty

let tree1 =
    Node(
        1,
        Node(5, Leaf, Node(4, Node(9, Leaf, Leaf), Node(2, Leaf, Leaf))),
        Node(3, Node(10, Leaf, Leaf), Node(6, Leaf, Leaf))
    )

// 4
amountOfTime tree1 3

// 0
amountOfTime (Node(1, Leaf, Leaf)) 1
