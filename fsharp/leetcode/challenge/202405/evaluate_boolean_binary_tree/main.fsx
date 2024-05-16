type Tree =
    | Leaf
    | Node of int * Tree * Tree

let rec evaluateTree (root: Tree) : bool =
    match root with
    | Leaf -> failwith "never reach here"
    | Node(v, Leaf, Leaf) -> v = 1
    | Node(_, _, Leaf)
    | Node(_, Leaf, _) -> failwith "never reach here"
    | Node(v, left, right) ->
        if v = 2 then
            (evaluateTree left) || (evaluateTree right)
        else
            (evaluateTree left) && (evaluateTree right)

let tree1 =
    Node(2, Node(1, Leaf, Leaf), Node(3, Node(0, Leaf, Leaf), Node(1, Leaf, Leaf)))
// true
evaluateTree tree1

let tree2 = Node(0, Leaf, Leaf)
// false
evaluateTree tree2
