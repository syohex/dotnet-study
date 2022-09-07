type Tree =
    | Leaf
    | Node of int * Tree * Tree

let tree2str (root: Tree) : string =
    let rec tree2str' node : string =
        match node with
        | Leaf -> ""
        | Node (v, left, right) ->
            let mutable ret = string v
            let tmp1 = tree2str' left
            let tmp2 = tree2str' right

            if tmp1.Length <> 0 then
                ret <- ret + "(" + tmp1 + ")"

            if tmp2.Length <> 0 then
                if tmp1.Length = 0 then
                    ret <- ret + "()"

                ret <- ret + "(" + tmp2 + ")"

            ret

    tree2str' root

let tree1 =
    Node(1, Node(2, Node(4, Leaf, Leaf), Leaf), Node(3, Leaf, Leaf))
// "1(2(4))(3)"
tree2str tree1

let tree2 =
    Node(1, Node(2, Leaf, Node(4, Leaf, Leaf)), Node(3, Leaf, Leaf))
// "1(2()(4))(3)"
tree2str tree2
