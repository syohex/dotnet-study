type Tree =
    | Leaf
    | Node of int * Tree * Tree

let pathSum (root: Tree) (targetSum: int) : int list list =
    let rec pathSum' node sum targetSum vals acc =
        match node with
        | Leaf ->
            if sum = targetSum then
                Set.add (vals |> List.rev) acc
            else
                acc
        | Node (v, left, right) ->
            let vals' = v :: vals
            let sum' = v + sum
            let acc' = pathSum' left sum' targetSum vals' acc
            pathSum' right sum' targetSum vals' acc'


    pathSum' root 0 targetSum [] Set.empty
    |> Set.toList

let tree1 =
    Node(
        5,
        Node(4, Node(11, Node(7, Leaf, Leaf), Node(2, Leaf, Leaf)), Leaf),
        Node(8, Node(13, Leaf, Leaf), Node(4, Node(5, Leaf, Leaf), Node(1, Leaf, Leaf)))
    )

// [[5,4,11,2],[5,8,4,5]]
pathSum tree1 22

let tree2 =
    Node(1, Node(2, Leaf, Leaf), Node(3, Leaf, Leaf))
// []
pathSum tree2 5

let tree3 = Node(1, Node(2, Leaf, Leaf), Leaf)
// []
pathSum tree3 0
