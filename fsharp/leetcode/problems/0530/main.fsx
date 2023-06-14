open System

type Tree =
    | Leaf
    | Node of int * Tree * Tree

let getMinimumDifference (root: Tree) =
    let rec collectValues node acc =
        match node with
        | Leaf -> acc
        | Node(v, left, right) ->
            let acc' = collectValues left (v :: acc)
            collectValues right acc'

    collectValues root []
    |> List.sort
    |> List.windowed 2
    |> List.fold
        (fun acc w ->
            match w with
            | n1 :: n2 :: [] -> Math.Min(acc, Math.Abs(n1 - n2))
            | _ -> failwith "never reach here")
        Int32.MaxValue

let tree1 =
    Node(4, Node(2, Node(1, Leaf, Leaf), Node(3, Leaf, Leaf)), Node(6, Leaf, Leaf))
// 1
getMinimumDifference tree1

let tree2 =
    Node(1, Node(0, Leaf, Leaf), Node(48, Node(12, Leaf, Leaf), Node(49, Leaf, Leaf)))
// 1
getMinimumDifference tree2
