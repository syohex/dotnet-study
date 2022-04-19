type Tree =
    | Leaf
    | Node of int * Tree * Tree

let collectValues (root: Tree) : int list =
    let rec collectValues' node acc =
        match node with
        | Leaf -> acc
        | Node (v, left, right) ->
            let acc' = collectValues' left acc
            collectValues' right (v :: acc')

    collectValues' root [] |> List.rev

let findSwapPair (nums: int list) : Map<int, int> =
    let createPairMap a b = Map.empty |> Map.add a b |> Map.add b a

    let rec findSwapPair' nums prev (first: Option<int>) (second: Option<int>) =
        match nums with
        | [] -> createPairMap first.Value second.Value
        | h :: t ->
            if prev <= h then
                findSwapPair' t h first second
            else
                let second' = Some h

                let first' =
                    if Option.isSome first then
                        first
                    else
                        Some prev

                findSwapPair' t h first' second'

    findSwapPair' (List.tail nums) (List.head nums) None None

let swapTree (root: Tree) (swaps: Map<int, int>) : Tree =
    let rec swapTree' root swaps =
        if Map.isEmpty swaps then
            root, Map.empty
        else
            match root with
            | Leaf -> Leaf, swaps
            | Node (v, left, right) ->
                match Map.tryFind v swaps with
                | Some (n) ->
                    let left', swaps' = swapTree' left (Map.remove v swaps)
                    let right', swaps'' = swapTree' right swaps'
                    Node(n, left', right'), swaps''
                | None ->
                    let left', swaps' = swapTree' left swaps
                    let right', swaps'' = swapTree' right swaps'
                    Node(v, left', right'), swaps''

    swapTree' root swaps |> fst

let recoverTree (root: Tree) : Tree =
    let vals = collectValues root
    let swaps = findSwapPair vals
    swapTree root swaps

let tree1 =
    Node(1, Node(3, Leaf, Node(2, Leaf, Leaf)), Leaf)
// [3,1,null,null,2]
recoverTree tree1

let tree2 =
    Node(3, Node(1, Leaf, Leaf), Node(4, Node(2, Leaf, Leaf), Leaf))
// [2,1,4,null,null,3]
recoverTree tree2
