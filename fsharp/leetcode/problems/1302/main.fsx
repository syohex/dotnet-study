open System

type Tree =
    | Leaf
    | Node of int * Tree * Tree

let deepestLeaveSum (root: Tree) : int =
    let rec deepestLeaveSum' node depth acc =
        match node with
        | Leaf -> depth - 1, acc
        | Node (v, left, right) ->
            let leftDepth, accLeft = deepestLeaveSum' left (depth + 1) acc
            let rightDepth, accRight = deepestLeaveSum' right (depth + 1) accLeft

            let acc' =
                match Map.tryFind depth accRight with
                | None -> Map.add depth v accRight
                | Some (n) -> Map.add depth (v + n) accRight

            Math.Max(leftDepth, rightDepth), acc'

    deepestLeaveSum' root 0 Map.empty ||> Map.find

let tree1 =
    Node(
        1,
        Node(2, Node(4, Node(7, Leaf, Leaf), Leaf), Node(5, Leaf, Leaf)),
        Node(3, Leaf, Node(6, Leaf, Node(8, Leaf, Leaf)))
    )

// 15
deepestLeaveSum tree1
