type Tree =
    | Leaf
    | Node of int * (Tree list)

let levelOrder (root: Tree) : int list list =
    let rec levelOrder' nodes acc =
        match nodes with
        | [] -> acc |> List.rev
        | _ ->
            let vals, children =
                nodes
                |> List.fold
                    (fun (vals, children) node ->
                        match node with
                        | Leaf -> vals, children
                        | Node (v, cs) -> v :: vals, children @ cs)
                    ([], [])

            levelOrder' children ((vals |> List.rev) :: acc)

    levelOrder' [ root ] []

let tree1 =
    Node(
        1,
        [ Node(3, [ Node(5, []); Node(6, []) ])
          Node(2, [])
          Node(4, []) ]
    )
// [[1],[3,2,4],[5,6]]
levelOrder tree1

let tree2 =
    Node(
        1,
        [ Node(2, [])
          Node(
              3,
              [ Node(6, [])
                Node(7, [ Node(11, [ Node(14, []) ]) ]) ]
          )
          Node(4, [ Node(8, [ Node(12, []) ]) ])
          Node(
              5,
              [ Node(9, [ Node(13, []) ])
                Node(10, []) ]
          ) ]
    )
// [[1],[2,3,4,5],[6,7,8,9,10],[11,12,13],[14]]
levelOrder tree2
