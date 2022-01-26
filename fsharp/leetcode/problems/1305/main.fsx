type TreeNode =
    | Empty
    | Node of int * TreeNode * TreeNode

let allElements (root: TreeNode) : int list =
    let rec allElements' (node: TreeNode) (acc: int list) : int list =
        match node with
        | Empty -> acc
        | Node (value, left, right) ->
            let left_acc = allElements' left (value :: acc)
            allElements' right left_acc

    allElements' root []

let getAllElements (root1: TreeNode) (root2: TreeNode) : int list =
    let values1 = allElements root1
    let values2 = allElements root2
    values1 @ values2 |> List.sort


let root1 =
    Node(2, Node(1, Empty, Empty), Node(4, Empty, Empty))

let root2 =
    Node(1, Node(0, Empty, Empty), Node(3, Empty, Empty))

let rootTest =
    Node(1, Node(0, Node(5, Empty, Empty), Empty), Node(3, Empty, Empty))

allElements root1
allElements rootTest

// [0;1;1;2;3;4]
getAllElements root1 root2

let root3 = Node(1, Empty, Node(8, Empty, Empty))
let root4 = Node(8, Node(1, Empty, Empty), Empty)

// [1;1;8;8]
getAllElements root3 root4
