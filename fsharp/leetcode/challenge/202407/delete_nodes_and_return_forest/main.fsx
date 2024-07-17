type Tree =
    | Leaf
    | Node of int * Tree * Tree

let delNodes (root: Tree) (toDelete: int list) : Tree list =
    let rec delNodes' node toDelete acc =
        match node with
        | Leaf -> Leaf, acc
        | Node(v, left, right) ->
            let left, acc = delNodes' left toDelete acc
            let right, acc = delNodes' right toDelete acc

            if Set.contains v toDelete then
                let acc = if left = Leaf then acc else left :: acc
                let acc = if right = Leaf then acc else right :: acc
                Leaf, acc
            else
                Node(v, left, right), acc

    let root, ret = delNodes' root (Set.ofList toDelete) []
    if root = Leaf then ret else root :: ret

let tree1 =
    Node(1, Node(2, Node(4, Leaf, Leaf), Node(5, Leaf, Leaf)), Node(3, Node(6, Leaf, Leaf), Node(7, Leaf, Leaf)))
// [[1,2,null,4],[6],[7]]
delNodes tree1 [ 3; 5 ]

let tree2 = Node(1, Node(2, Leaf, Node(3, Leaf, Leaf)), Node(4, Leaf, Leaf))
// [[1,2,4]]
delNodes tree2 [ 3 ]
