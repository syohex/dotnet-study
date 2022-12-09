open System

type Tree =
    | Leaf
    | Node of int * Tree * Tree

let maxAncestorDiff (root: Tree) : int =
    let rec maxAncestorDiff' node min max =
        match node with
        | Leaf -> 0
        | Node(v, left, right) ->
            let min' = Math.Min(min, v)
            let max' = Math.Max(max, v)
            let diff = Math.Max(Math.Abs(v - min), Math.Abs(v - max))
            let leftVal = maxAncestorDiff' left min' max'
            let rightVal = maxAncestorDiff' right min' max'

            Math.Max(diff, Math.Max(leftVal, rightVal))

    match root with
    | Leaf -> failwith "never reach here"
    | Node(v, _, _) -> maxAncestorDiff' root v v

let tree1 =
    Node(
        8,
        Node(3, Node(1, Leaf, Leaf), Node(6, Node(4, Leaf, Leaf), Node(7, Leaf, Leaf))),
        Node(10, Leaf, Node(14, Node(13, Leaf, Leaf), Leaf))
    )
// 7
maxAncestorDiff tree1

let tree2 = Node(1, Leaf, Node(2, Leaf, Node(0, Node(3, Leaf, Leaf), Leaf)))
// 3
maxAncestorDiff tree2
