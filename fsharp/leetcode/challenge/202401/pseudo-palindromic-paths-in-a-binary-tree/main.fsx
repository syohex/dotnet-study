type Tree =
    | Leaf
    | Node of int * Tree * Tree

let isPseudoPalindrom m len =
    let odd = Map.values m |> Seq.filter (fun n -> n % 2 <> 0) |> Seq.length
    if len % 2 = 0 then odd = 0 else odd = 1

let pseudoPalindromicPaths (root: Tree) : int =
    let rec pseudoPalindromicPaths' node m len =
        match node with
        | Leaf -> 0
        | Node(v, Leaf, Leaf) ->
            let m' =
                match Map.tryFind v m with
                | Some(a) -> Map.add v (a + 1) m
                | None -> Map.add v 1 m

            if isPseudoPalindrom m' len then 1 else 0
        | Node(v, left, right) ->
            let m' =
                match Map.tryFind v m with
                | Some(a) -> Map.add v (a + 1) m
                | None -> Map.add v 1 m

            let a = pseudoPalindromicPaths' left m' (len + 1)
            let b = pseudoPalindromicPaths' right m' (len + 1)
            a + b

    pseudoPalindromicPaths' root Map.empty 1

let tree1 =
    Node(2, Node(3, Node(3, Leaf, Leaf), Node(1, Leaf, Leaf)), Node(1, Leaf, Node(1, Leaf, Leaf)))
// 2
pseudoPalindromicPaths tree1

let tree2 =
    Node(2, Node(1, Node(1, Leaf, Leaf), Node(3, Leaf, Node(1, Leaf, Leaf))), Node(1, Leaf, Leaf))
// 1
pseudoPalindromicPaths tree2

let tree3 = Node(9, Leaf, Leaf)
// 1
pseudoPalindromicPaths tree3
