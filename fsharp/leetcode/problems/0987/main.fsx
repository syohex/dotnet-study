type Tree =
    | Leaf
    | Node of int * Tree * Tree

let verticalTraversal (root: Tree) : int list list =
    let rec verticalTraversal' node row col acc =
        match node with
        | Leaf -> acc
        | Node (v, left, right) ->
            let acc' =
                match Map.tryFind col acc with
                | None -> Map.add col (Map.add row [ v ] Map.empty) acc
                | Some (m) ->
                    match Map.tryFind row m with
                    | None -> Map.add col (Map.add row [ v ] m) acc
                    | Some (vals) -> Map.add col (Map.add row ((v :: vals) |> List.sort) m) acc

            let acc'' =
                verticalTraversal' left (row + 1) (col - 1) acc'

            verticalTraversal' right (row + 1) (col + 1) acc''

    let rec f cols m acc =
        match cols with
        | [] -> acc |> List.rev
        | h :: t ->
            let cols = Map.find h m

            let rows =
                Map.keys cols
                |> Seq.sort
                |> Seq.fold
                    (fun a i ->
                        let vals = Map.find i cols
                        a @ vals)
                    []

            f t m (rows :: acc)

    let m = verticalTraversal' root 0 0 Map.empty
    let cols = m |> Map.keys |> Seq.sort |> Seq.toList
    f cols m []

let tree1 =
    Node(3, Node(9, Leaf, Leaf), Node(20, Node(15, Leaf, Leaf), Node(7, Leaf, Leaf)))
// [[9],[3,15],[20],[7]]
verticalTraversal tree1

let tree2 =
    Node(1, Node(2, Node(4, Leaf, Leaf), Node(5, Leaf, Leaf)), Node(3, Node(6, Leaf, Leaf), Node(7, Leaf, Leaf)))
// [[4],[2],[1,5,6],[3],[7]]
verticalTraversal tree2

let tree3 =
    Node(1, Node(2, Node(4, Leaf, Leaf), Node(6, Leaf, Leaf)), Node(3, Node(5, Leaf, Leaf), Node(7, Leaf, Leaf)))
// [[4],[2],[1,5,6],[3],[7]]
verticalTraversal tree3
