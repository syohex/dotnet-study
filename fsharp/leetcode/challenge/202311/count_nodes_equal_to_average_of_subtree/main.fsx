type Tree =
    | Leaf
    | Node of int * Tree * Tree

let averageOfSubtree (root: Tree) : int =
    let rec averageOfSubtree' node =
        match node with
        | Leaf -> 0, 0, 0
        | Node(v, left, right) ->
            let ret1, sum1, count1 = averageOfSubtree' left
            let ret2, sum2, count2 = averageOfSubtree' right

            let count = 1 + count1 + count2
            let sum = v + sum1 + sum2
            let average = sum / count

            let ret = if v = average then ret1 + ret2 + 1 else ret1 + ret2
            ret, sum, count

    let ret, _, _ = averageOfSubtree' root
    ret

let tree1 =
    Node(4, Node(8, Node(0, Leaf, Leaf), Node(1, Leaf, Leaf)), Node(5, Leaf, Node(6, Leaf, Leaf)))
// 5
averageOfSubtree tree1

let tree2 = Node(1, Leaf, Leaf)
// 1
averageOfSubtree tree2
