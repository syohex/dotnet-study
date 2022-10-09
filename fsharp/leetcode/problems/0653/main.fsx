type Tree =
    | Leaf
    | Node of int * Tree * Tree

let findTarget (root: Tree) (k: int) : bool =
    let rec collectValues node vals =
        match node with
        | Leaf -> vals
        | Node (v, left, right) ->
            let vals' =
                match Map.tryFind v vals with
                | Some (w) -> Map.add v (w + 1) vals
                | None -> Map.add v 1 vals

            let vals'' = collectValues left vals'
            collectValues right vals''

    let vals = collectValues root Map.empty

    vals
    |> Map.fold
        (fun acc key _ ->
            if acc then
                acc
            else
                let diff = k - key

                match Map.tryFind diff vals with
                | None -> false
                | Some (w) -> if key = diff then w >= 2 else true)
        false

let tree1 =
    Node(5, Node(3, Node(2, Leaf, Leaf), Node(4, Leaf, Leaf)), Node(6, Leaf, Node(7, Leaf, Leaf)))
// true
findTarget tree1 9

// false
findTarget tree1 28


let tree2 =
    Node(5, Node(1, Node(1, Leaf, Leaf), Node(1, Leaf, Leaf)), Node(1, Leaf, Node(1, Leaf, Leaf)))
// false
findTarget tree2 10

let tree3 =
    Node(5, Node(1, Node(1, Leaf, Leaf), Node(1, Leaf, Leaf)), Node(1, Leaf, Node(5, Leaf, Leaf)))
// true
findTarget tree3 10
