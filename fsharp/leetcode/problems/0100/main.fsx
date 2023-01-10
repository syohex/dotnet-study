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

let p1 = Node(1, Node(2, Leaf, Leaf), Node(3, Leaf, Leaf))
let q1 = Node(1, Node(2, Leaf, Leaf), Node(3, Leaf, Leaf))
// true
isSameTree p1 q1

let p2 = Node(1, Node(2, Leaf, Leaf), Leaf)
let q2 = Node(1, Leaf, Node(2, Leaf, Leaf))
// false
isSameTree p2 q2

let p3 = Node(1, Node(2, Leaf, Leaf), Node(1, Leaf, Leaf))
let q3 = Node(1, Node(1, Leaf, Leaf), Node(2, Leaf, Leaf))
// false
isSameTree p3 q3
