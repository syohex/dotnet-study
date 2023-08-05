type Tree =
    | Leaf
    | Node of int * Tree * Tree

let generateTrees (n: int) : Tree list =
    let rec combinations a b acc =
        match a with
        | [] -> List.rev acc
        | h :: t ->
            let acc' = b |> List.fold (fun acc e -> (h, e) :: acc) acc
            combinations t b acc'

    let rec generateTrees' left right =
        if left > right then
            [ Leaf ]
        else
            seq { left..right }
            |> Seq.fold
                (fun acc n ->
                    let leftTrees = generateTrees' left (n - 1)
                    let rightTrees = generateTrees' (n + 1) right

                    combinations leftTrees rightTrees []
                    |> List.fold (fun acc (leftTree, rightTree) -> (Node(n, leftTree, rightTree) :: acc)) acc)
                []

    generateTrees' 1 n |> List.rev

// [[1,null,2,null,3],[1,null,3,2],[2,1,3],[3,1,null,null,2],[3,2,null,1]]
generateTrees 3

// [[1]]
generateTrees 1
