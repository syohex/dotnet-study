type Tree =
    | Leaf
    | Node of int * Tree * Tree

let collectValues (root: Tree) : int list =
    let rec collectValues' node acc =
        match node with
        | Leaf -> acc
        | Node(v, left, right) ->
            let acc = collectValues' left acc
            collectValues' right (v :: acc)

    collectValues' root [] |> List.rev

let balanceBST (root: Tree) : Tree =
    let rec balanceBST' left right (v: int[]) =
        if left > right then
            Leaf
        else
            let mid = left + (right - left) / 2
            let leftTree = balanceBST' left (mid - 1) v
            let rightTree = balanceBST' (mid + 1) right v

            Node(v.[mid], leftTree, rightTree)

    let v = collectValues root |> List.toArray
    balanceBST' 0 (v.Length - 1) v

let tree1 = Node(1, Leaf, Node(2, Leaf, Node(3, Leaf, Node(4, Leaf, Leaf))))
balanceBST tree1

let tree2 = Node(2, Node(1, Leaf, Leaf), Node(3, Leaf, Leaf))
balanceBST tree2
