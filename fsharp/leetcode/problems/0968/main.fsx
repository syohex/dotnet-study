open System

type Tree =
    | Leaf
    | Node of int * Tree * Tree

let minCameraCover (root: Tree) : int =
    let rec minCameraCover' node =
        match node with
        | Leaf -> (0, 0, 2000)
        | Node (_, left, right) ->
            let left0, left1, left2 = minCameraCover' left
            let right0, right1, right2 = minCameraCover' right

            let leftMin = Math.Min(left1, left2)
            let rightMin = Math.Min(right1, right2)

            let ret0 = left1 + right1

            let ret1 =
                Math.Min(left2 + rightMin, right2 + leftMin)

            let ret2 =
                1
                + Math.Min(left0, leftMin)
                + Math.Min(right0, rightMin)

            ret0, ret1, ret2

    let _, ret1, ret2 = minCameraCover' root
    Math.Min(ret1, ret2)

// 1
minCameraCover (Node(0, Node(0, Node(0, Leaf, Leaf), Node(0, Leaf, Leaf)), Leaf))

// 2
minCameraCover (Node(0, Node(0, Node(0, Node(0, Leaf, Node(0, Leaf, Leaf)), Leaf), Leaf), Leaf))
