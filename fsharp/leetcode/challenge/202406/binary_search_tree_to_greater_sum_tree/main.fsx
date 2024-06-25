type Tree =
    | Leaf
    | Node of int * Tree * Tree

let bstToGst (root: Tree) : Tree =
    let rec bstToGst' node sum =
        match node with
        | Leaf -> Leaf, sum
        | Node(v, left, right) ->
            let right', sumRight =
                match right with
                | Leaf -> right, sum
                | Node(_) -> bstToGst' right sum

            let sum = sumRight + v

            let left', sumLeft =
                match left with
                | Leaf -> left, sum
                | Node(_) -> bstToGst' left sum

            Node(sum, left', right'), sumLeft

    bstToGst' root 0 |> fst

let tree1 =
    Node(
        4,
        Node(1, Node(0, Leaf, Leaf), Node(2, Leaf, Node(3, Leaf, Leaf))),
        Node(6, Node(5, Leaf, Leaf), Node(7, Leaf, Node(8, Leaf, Leaf)))
    )
// [30,36,21,36,35,26,15,null,null,null,33,null,null,null,8]
bstToGst tree1

let tree2 = Node(0, Leaf, Node(1, Leaf, Leaf))
// [1,null,1]
bstToGst tree2
