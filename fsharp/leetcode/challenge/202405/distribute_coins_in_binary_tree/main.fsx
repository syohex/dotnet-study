type Tree =
    | Leaf
    | Node of int * Tree * Tree

let distributeCoins (root: Tree) : int =
    let rec distributeCoins' node =
        match node with
        | Leaf -> 0, 0
        | Node(v, left, right) ->
            let lv, lMoves = distributeCoins' left
            let rv, rMoves = distributeCoins' right

            let moves = abs lv + abs rv + lMoves + rMoves
            v - 1 + lv + rv, moves

    distributeCoins' root |> snd

let tree1 = Node(3, Node(0, Leaf, Leaf), Node(0, Leaf, Leaf))
// 2
distributeCoins tree1

let tree2 = Node(0, Node(3, Leaf, Leaf), Node(0, Leaf, Leaf))
// 3
distributeCoins tree2
