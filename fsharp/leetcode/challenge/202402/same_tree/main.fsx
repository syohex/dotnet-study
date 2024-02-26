type Tree =
    | Leaf
    | Node of int * Tree * Tree

let rec isSameTree (p: Tree) (q: Tree) : bool =
    match p, q with
    | Leaf, Leaf -> true
    | Node(_), Leaf
    | Leaf, Node(_) -> false
    | Node(v1, left1, right1), Node(v2, left2, right2) ->
        if v1 <> v2 then
            false
        else
            isSameTree left1 left2 && isSameTree right1 right2

let tree11 = Node(1, Node(2, Leaf, Leaf), Node(3, Leaf, Leaf))
let tree12 = Node(1, Node(2, Leaf, Leaf), Node(3, Leaf, Leaf))
// true
isSameTree tree11 tree12

let tree21 = Node(1, Node(2, Leaf, Leaf), Leaf)
let tree22 = Node(1, Leaf, Node(2, Leaf, Leaf))
// false
isSameTree tree21 tree22

let tree31 = Node(1, Node(2, Leaf, Leaf), Node(1, Leaf, Leaf))
let tree32 = Node(1, Node(1, Leaf, Leaf), Node(2, Leaf, Leaf))
// false
isSameTree tree31 tree32
