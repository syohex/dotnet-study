type Tree =
    | Leaf
    | Node of int * Tree * Tree

let rec removeLeafNodes (root: Tree) (target: int) : Tree =
    match root with
    | Leaf -> Leaf
    | Node(v, Leaf, Leaf) when v = target -> Leaf
    | Node(v, left, right) ->
        let left' = removeLeafNodes left target
        let right' = removeLeafNodes right target

        match left', right', v = target with
        | Leaf, Leaf, true -> Leaf
        | _ -> Node(v, left', right')

let tree1 =
    Node(1, Node(2, Node(2, Leaf, Leaf), Leaf), Node(3, Node(2, Leaf, Leaf), Node(4, Leaf, Leaf)))

removeLeafNodes tree1 2

let tree2 =
    Node(1, Node(3, Node(3, Leaf, Leaf), Node(2, Leaf, Leaf)), Node(3, Leaf, Leaf))

removeLeafNodes tree2 3

let tree3 = Node(1, Node(2, Node(2, Node(2, Leaf, Leaf), Leaf), Leaf), Leaf)
removeLeafNodes tree3 2
