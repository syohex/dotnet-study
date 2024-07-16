open System

type Tree =
    | Leaf
    | Node of int * Tree * Tree

let getDirections (root: Tree) (startValue: int) (destValue: int) : string =
    let rec findPath node value acc =
        match node with
        | Leaf -> []
        | Node(v, left, right) ->
            if v = value then
                List.rev acc
            else
                match findPath left value ('L' :: acc) with
                | [] -> findPath right value ('R' :: acc)
                | ret -> ret

    let rec getDirections' (startPath: char list) (destPath: char list) =
        match startPath, destPath with
        | [], [] -> ""
        | s, [] -> List.init (List.length s) (fun _ -> 'U') |> String.Concat
        | [], d -> d |> String.Concat
        | h1 :: t1, h2 :: t2 ->
            if h1 = h2 then
                getDirections' t1 t2
            else
                List.init (List.length startPath) (fun _ -> 'U') @ destPath |> String.Concat

    let startPath = findPath root startValue []
    let destPath = findPath root destValue []

    getDirections' startPath destPath

let tree1 =
    Node(5, Node(1, Node(3, Leaf, Leaf), Leaf), Node(2, Node(6, Leaf, Leaf), Node(4, Leaf, Leaf)))
// "UURL"
getDirections tree1 3 6

let tree2 = Node(2, Node(1, Leaf, Leaf), Leaf)
// "L"
getDirections tree2 2 1

let tree3 =
    Node(5, Node(8, Node(1, Leaf, Leaf), Leaf), Node(3, Node(4, Leaf, Leaf), Node(7, Leaf, Leaf)))
// "U"
getDirections tree3 4 3
