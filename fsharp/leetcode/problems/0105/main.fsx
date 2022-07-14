type Tree =
    | Leaf
    | Node of int * Tree * Tree

let buildTree (preorder: int list) (inorder: int list) : Tree =
    let rec buildTree' preorder left right inorderIndex =
        if left > right then
            Leaf, preorder
        else
            match preorder with
            | [] -> Leaf, []
            | h :: t ->
                let index = Map.find h inorderIndex

                let left, preorder' =
                    buildTree' t left (index - 1) inorderIndex

                let right, preorder'' =
                    buildTree' preorder' (index + 1) right inorderIndex

                Node(h, left, right), preorder''

    let inorderIndex =
        inorder
        |> List.mapi (fun i v -> i, v)
        |> List.fold (fun acc (i, v) -> Map.add v i acc) Map.empty

    buildTree' preorder 0 (inorder.Length - 1) inorderIndex
    |> fst

//  [3,9,20,null,null,15,7]
buildTree [ 3; 9; 20; 15; 7 ] [
    9
    3
    15
    20
    7
]

// [-1]
buildTree [ -1 ] [ -1 ]
