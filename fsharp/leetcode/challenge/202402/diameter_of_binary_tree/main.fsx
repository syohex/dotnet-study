open System

type Tree =
    | Leaf
    | Node of int * Tree * Tree

let diameterOfBinaryTree (root: Tree) : int =
    let rec diameterOfBinaryTree' node =
        match node with
        | Leaf -> 0, 0
        | Node(_, left, right) ->
            let leftLen, leftMax = diameterOfBinaryTree' left
            let rightLen, rightMax = diameterOfBinaryTree' right

            let longest = Math.Max(leftLen + rightLen, Math.Max(leftMax, rightMax))
            1 + System.Math.Max(leftLen, rightLen), longest

    diameterOfBinaryTree' root |> snd

let tree1 =
    Node(1, Node(2, Node(4, Leaf, Leaf), Node(5, Leaf, Leaf)), Node(3, Leaf, Leaf))
// 3
diameterOfBinaryTree tree1

let tree2 = Node(1, Node(2, Leaf, Leaf), Leaf)
// 1
diameterOfBinaryTree tree2
