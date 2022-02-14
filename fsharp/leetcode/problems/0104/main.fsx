type Tree =
    | Leaf
    | TreeNode of int * Tree * Tree

let rec maxDepth (r: Tree) : int =
    match r with
    | Leaf -> 0
    | TreeNode (_, left, right) -> (max (maxDepth left) (maxDepth right)) + 1

// 3
let root1 =
    TreeNode(3, TreeNode(9, Leaf, Leaf), TreeNode(20, TreeNode(15, Leaf, Leaf), TreeNode(7, Leaf, Leaf)))

maxDepth root1

// 2
let root2 =
    TreeNode(1, Leaf, TreeNode(2, Leaf, Leaf))

maxDepth root2

// 0
maxDepth Leaf
