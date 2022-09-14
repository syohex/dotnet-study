type Tree =
    | Leaf
    | Node of int * Tree * Tree

let pseudoPalindromicPaths (root: Tree) : int =
    let isPseudoPalindrome m =
        let odds =
            m
            |> Map.fold (fun acc _ v -> if v % 2 = 1 then acc + 1 else acc) 0

        odds <= 1

    let rec pseudoPalindromicPaths' node freq =
        match node with
        | Leaf -> failwith "never reach here"
        | Node (v, left, right) ->
            let freq' =
                match Map.tryFind v freq with
                | None -> Map.add v 1 freq
                | Some (x) -> Map.add v (x + 1) freq

            match left, right with
            | Leaf, Leaf ->
                if isPseudoPalindrome freq' then
                    1
                else
                    0
            | Node (_), Leaf -> pseudoPalindromicPaths' left freq'
            | Leaf, Node (_) -> pseudoPalindromicPaths' right freq'
            | Node (_), Node (_) ->
                let left = pseudoPalindromicPaths' left freq'
                let right = pseudoPalindromicPaths' right freq'
                left + right

    pseudoPalindromicPaths' root Map.empty

let tree1 =
    Node(2, Node(3, Node(3, Leaf, Leaf), Node(1, Leaf, Leaf)), Node(1, Leaf, Node(1, Leaf, Leaf)))
// 2
pseudoPalindromicPaths tree1

let tree2 =
    Node(2, Node(1, Node(1, Leaf, Leaf), Node(3, Leaf, Node(1, Leaf, Leaf))), Node(1, Leaf, Leaf))
// 1
pseudoPalindromicPaths tree2
