type Tree =
    | Leaf
    | Node of int * Tree * Tree

let averageOfSubTree (root: Tree) : int =
    let rec averageOfSubTree' (node: Tree) : (int * int * int) =
        match node with
        | Leaf -> 0, 0, 0
        | Node (v, left, right) ->
            let (leftSum, leftNodes, leftRet) = averageOfSubTree' left
            let (rightSum, rightNodes, rightRet) = averageOfSubTree' right

            let sum = v + leftSum + rightSum
            let nodes = 1 + leftNodes + rightNodes
            let average = sum / nodes
            let ret = leftRet + rightRet

            sum, nodes, (if v = average then ret + 1 else ret)

    let _, _, ret = averageOfSubTree' root
    ret

let tree1 =
    Node(4, Node(8, Node(0, Leaf, Leaf), Node(1, Leaf, Leaf)), Node(5, Leaf, Node(6, Leaf, Leaf)))

// 5
averageOfSubTree tree1

let tree2 = Node(1, Leaf, Leaf)

// 1
averageOfSubTree tree2
