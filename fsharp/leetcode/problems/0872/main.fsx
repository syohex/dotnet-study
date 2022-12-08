type Tree =
    | Leaf
    | Node of int * Tree * Tree

let leafSimilar (root1: Tree) (root2: Tree) : bool =
    let rec collectLeaves node acc =
        match node with
        | Leaf -> acc
        | Node(v, Leaf, Leaf) -> v :: acc
        | Node(_, left, right) ->
            let acc' = collectLeaves left acc
            collectLeaves right acc'

    let leaves1 = collectLeaves root1 [] |> List.rev
    let leaves2 = collectLeaves root2 [] |> List.rev

    leaves1 = leaves2

let tree11 =
    Node(
        3,
        Node(5, Node(6, Leaf, Leaf), Node(2, Node(7, Leaf, Leaf), Node(4, Leaf, Leaf))),
        Node(1, Node(9, Leaf, Leaf), Node(8, Leaf, Leaf))
    )

let tree12 =
    Node(
        3,
        Node(5, Node(6, Leaf, Leaf), Node(7, Leaf, Leaf)),
        Node(1, Node(4, Leaf, Leaf), Node(2, Node(9, Leaf, Leaf), Node(8, Leaf, Leaf)))
    )
// true
leafSimilar tree11 tree12

let tree21 = Node(1, Node(2, Leaf, Leaf), Node(3, Leaf, Leaf))
let tree22 = Node(1, Node(3, Leaf, Leaf), Node(2, Leaf, Leaf))
// false
leafSimilar tree21 tree22
