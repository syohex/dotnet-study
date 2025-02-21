type Tree =
    | Leaf
    | Node of int * Tree * Tree

type FindElement =
    { Vals: Set<int> }

    static member init(root: Tree) : FindElement =
        { Vals = FindElement.initValues 0 root Set.empty }

    static member initValues v node acc =
        let acc = Set.add v acc

        match node with
        | Leaf -> acc
        | Node(_, left, right) ->
            let acc =
                match left with
                | Leaf -> acc
                | _ -> FindElement.initValues (2 * v + 1) left acc

            match right with
            | Leaf -> acc
            | _ -> FindElement.initValues (2 * v + 2) right acc

    static member find v (fe: FindElement) : bool = Set.contains v fe.Vals

let tree1 = Node(-1, Leaf, Node(-1, Leaf, Leaf))
let fe1 = FindElement.init tree1
// false
FindElement.find 1 fe1
// true
FindElement.find 2 fe1

let tree2 =
    Node(-1, Node(-1, Node(-1, Leaf, Leaf), Node(-1, Leaf, Leaf)), Node(-1, Leaf, Leaf))

let fe2 = FindElement.init tree2
// true
FindElement.find 1 fe2
// true
FindElement.find 3 fe2
// false
FindElement.find 5 fe2

let tree3 = Node(-1, Leaf, Node(-1, Node(-1, Node(-1, Leaf, Leaf), Leaf), Leaf))
let fe3 = FindElement.init tree3
// true
FindElement.find 2 fe3
// false
FindElement.find 3 fe3
// false
FindElement.find 4 fe3
// true
FindElement.find 5 fe3
