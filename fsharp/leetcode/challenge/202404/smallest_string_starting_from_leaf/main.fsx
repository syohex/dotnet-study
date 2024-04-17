type Tree =
    | Leaf
    | Node of int * Tree * Tree

let smallestFromLeaf (root: Tree) : string =
    let rec smallestFromLeaf' node acc ret =
        match node with
        | Leaf -> ret
        | Node(v, left, right) ->
            let acc' = v :: acc

            if left = Leaf && right = Leaf then
                match ret with
                | [] -> acc'
                | _ -> if acc' < ret then acc' else ret
            else
                let ret = smallestFromLeaf' left acc' ret
                smallestFromLeaf' right acc' ret

    smallestFromLeaf' root [] []
    |> List.map (fun i -> i + int 'a' |> char)
    |> System.String.Concat

let tree1 =
    Node(0, Node(1, Node(3, Leaf, Leaf), Node(4, Leaf, Leaf)), Node(2, Node(3, Leaf, Leaf), Node(4, Leaf, Leaf)))
// "dba"
smallestFromLeaf tree1

let tree2 =
    Node(25, Node(1, Node(1, Leaf, Leaf), Node(3, Leaf, Leaf)), Node(3, Node(0, Leaf, Leaf), Node(2, Leaf, Leaf)))
// "adz"
smallestFromLeaf tree2
