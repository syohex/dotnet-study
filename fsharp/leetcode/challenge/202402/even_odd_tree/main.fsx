type Tree =
    | Leaf
    | Node of int * Tree * Tree

let isEvenOddTree (root: Tree) : bool =
    let rec isEvenOddTree' q isEven =
        match q with
        | [] -> true
        | _ ->
            let prev = if isEven then -1 else System.Int32.MaxValue

            let q', _, ok =
                q
                |> List.fold
                    (fun (acc, prev, ok) node ->
                        if not ok then
                            acc, prev, false
                        else
                            match node with
                            | Leaf -> failwith "never reach here"
                            | Node(v, left, right) ->
                                if isEven then
                                    if v % 2 = 1 && prev < v then
                                        right :: left :: acc, v, true
                                    else
                                        acc, v, false
                                else if v % 2 = 0 && prev > v then
                                    right :: left :: acc, v, true
                                else
                                    acc, v, false)
                    ([], prev, true)

            if ok then
                let q'' = q' |> List.filter (fun node -> node <> Leaf) |> List.rev
                isEvenOddTree' q'' (not isEven)
            else
                false

    isEvenOddTree' [ root ] true

let tree1 =
    Node(
        1,
        Node(10, Node(3, Node(12, Leaf, Leaf), Node(8, Leaf, Leaf)), Leaf),
        Node(4, Node(7, Node(6, Leaf, Leaf), Leaf), Node(9, Leaf, Node(2, Leaf, Leaf)))
    )
// true
isEvenOddTree tree1

let tree2 =
    Node(5, Node(4, Node(3, Leaf, Leaf), Node(3, Leaf, Leaf)), Node(2, Node(7, Leaf, Leaf), Leaf))
// false
isEvenOddTree tree2

let tree3 =
    Node(5, Node(9, Node(3, Leaf, Leaf), Node(5, Leaf, Leaf)), Node(1, Node(7, Leaf, Leaf), Leaf))
// false
isEvenOddTree tree3
