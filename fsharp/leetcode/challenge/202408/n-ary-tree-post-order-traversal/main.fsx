type Node =
    | Leaf
    | Node of int * Node list

let postorder (root: Node) : int list =
    let rec postorder' node acc =
        match node with
        | Leaf -> acc
        | Node(v, children) ->
            let acc = children |> List.fold (fun acc child -> postorder' child acc) acc
            v :: acc

    postorder' root [] |> List.rev

let tree1 =
    Node(1, [ Node(3, [ Node(5, []); Node(6, []) ]); Node(2, []); Node(4, []) ])
// [5,6,3,2,4,1]
postorder tree1

let tree2 =
    Node(
        1,
        [ Node(2, [])
          Node(3, [ Node(6, []); Node(7, [ Node(11, [ Node(14, []) ]) ]) ])
          Node(4, [ Node(8, [ Node(12, []) ]) ])
          Node(5, [ Node(9, [ Node(13, []) ]); Node(10, []) ]) ]
    )
// [2,6,14,11,7,3,12,8,4,13,9,10,5,1]
postorder tree2
