type Tree =
    | Leaf
    | Node of int * Tree * Tree

let rec getTargetCopy (original: Tree) (cloned: Tree) (target: int) : Tree =
    match original, cloned with
    | Leaf, Leaf -> Leaf
    | Node (v1, left1, right1), Node (_, left2, right2) ->
        if v1 = target then
            cloned
        else
            match getTargetCopy left1 left2 target with
            | Node (_, _, _) as r -> r
            | Leaf -> getTargetCopy right1 right2 target
    | _, _ -> failwith "never reach here"

let tree1 =
    Node(7, Node(4, Leaf, Leaf), Node(3, Node(6, Leaf, Leaf), Node(19, Leaf, Leaf)))

let cloned1 =
    Node(7, Node(4, Leaf, Leaf), Node(3, Node(6, Leaf, Leaf), Node(19, Leaf, Leaf)))

getTargetCopy tree1 cloned1 3

let tree2 = Node(7, Leaf, Leaf)
let cloned2 = Node(7, Leaf, Leaf)

getTargetCopy tree2 cloned2 7
