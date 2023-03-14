type Tree =
    | Leaf
    | Node of int * Tree * Tree

let sumNumbers (root: Tree) : int =
    let rec sumNumbers' node acc =
        match node with
        | Leaf -> failwith "never reach here"
        | Node(v, left, right) ->
            let acc' = acc * 10 + v

            match left, right with
            | Leaf, Leaf -> acc'
            | _, Leaf -> sumNumbers' left acc'
            | Leaf, _ -> sumNumbers' right acc'
            | _ -> (sumNumbers' left acc') + (sumNumbers' right acc')

    sumNumbers' root 0

let tree1 = Node(1, Node(2, Leaf, Leaf), Node(3, Leaf, Leaf))
// 25
sumNumbers tree1

let tree2 =
    Node(4, Node(9, Node(5, Leaf, Leaf), Node(1, Leaf, Leaf)), Node(0, Leaf, Leaf))
// 1026
sumNumbers tree2
