type Tree =
    | Leaf
    | Node of int * Tree * Tree

let goodNodes (root: Tree) : int =
    let rec goodNodes' node max =
        match node with
        | Leaf -> 0
        | Node (v, left, right) ->
            let ret = if v >= max then 1 else 0
            let max' = System.Math.Max(max, v)

            ret
            + (goodNodes' left max')
            + (goodNodes' right max')

    goodNodes' root System.Int32.MinValue

let tree1 =
    Node(3, Node(1, Node(3, Leaf, Leaf), Leaf), Node(4, Node(1, Leaf, Leaf), Node(5, Leaf, Leaf)))
// 4
goodNodes tree1

let tree2 =
    Node(3, Node(3, Node(4, Leaf, Leaf), Node(2, Leaf, Leaf)), Leaf)
// 3
goodNodes tree2
