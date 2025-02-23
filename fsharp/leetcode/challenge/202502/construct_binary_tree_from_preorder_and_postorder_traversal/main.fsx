type Tree =
    | Leaf
    | Node of int * Tree * Tree

let constructFromPrePost (preorder: int[]) (postorder: int[]) : Tree =
    let rec f preStart preEnd postStart =
        if preStart > preEnd then
            Leaf
        elif preStart = preEnd then
            Node(preorder.[preStart], Leaf, Leaf)
        else
            let leftNodePos = postorder |> Array.findIndex ((=) preorder.[preStart + 1])
            let leftNodes = leftNodePos - postStart + 1
            let left = f (preStart + 1) (preStart + leftNodes) postStart
            let right = f (preStart + leftNodes + 1) preEnd (postStart + leftNodes + 1)
            Node(preorder.[preStart], left, right)

    f 0 (preorder.Length - 1) 0

// [1,2,3,4,5,6,7]
constructFromPrePost [| 1; 2; 4; 5; 3; 6; 7 |] [| 4; 5; 2; 6; 7; 3; 1 |]

// [1]
constructFromPrePost [| 1 |] [| 1 |]
