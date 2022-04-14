type Tree =
    | Leaf
    | TreeNode of int * Tree * Tree

let rec searchBST (root: Tree) (value: int) : Tree =
    match root with
    | Leaf -> Leaf
    | TreeNode (v, left, right) ->
        if v = value then
            root
        else if value < v then
            searchBST left value
        else
            searchBST right value

let tree1 =
    TreeNode(4, TreeNode(2, TreeNode(1, Leaf, Leaf), TreeNode(3, Leaf, Leaf)), TreeNode(7, Leaf, Leaf))

// [2,1,3]
searchBST tree1 2

// Leaf
searchBST tree1 5
