type Tree =
    | Leaf
    | Node of int * Tree * Tree

let kthLongestLevelSum (root: Tree) (k: int) : int =
    let rec f node level acc =
        match node with
        | Leaf -> acc
        | Node(v, left, right) ->
            let sum = Map.tryFind level acc |> Option.defaultValue 0
            let acc = Map.add level (sum + v) acc
            let acc = f left (level + 1) acc
            f right (level + 1) acc

    f root 0 Map.empty
    |> Map.values
    |> Seq.sort
    |> Seq.rev
    |> Seq.tryItem (k - 1)
    |> Option.defaultValue -1

let tree1 =
    Node(
        5,
        Node(8, Node(2, Node(4, Leaf, Leaf), Node(6, Leaf, Leaf)), Node(1, Leaf, Leaf)),
        Node(9, Node(3, Leaf, Leaf), Node(7, Leaf, Leaf))
    )
// 13
kthLongestLevelSum tree1 2

// -1
kthLongestLevelSum tree1 50

let tree2 = Node(1, Node(2, Node(3, Leaf, Leaf), Leaf), Leaf)
// 3
kthLongestLevelSum tree2 1
