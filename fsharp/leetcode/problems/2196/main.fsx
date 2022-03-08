type BinaryTree =
    | TreeEnd
    | TreeNode of int * BinaryTree * BinaryTree

type GraphNode =
    { Left: Option<int>
      Right: Option<int> }

let descriptionsToGraph (descriptions: (int * int * int) list) : Map<int, GraphNode> =
    let rec descriptionsToGraph' descs graph =
        match descs with
        | [] -> graph
        | (parent, child, is_left) :: rest ->
            match Map.tryFind parent graph with
            | Some (n) ->
                if is_left = 1 then
                    descriptionsToGraph' rest (Map.add parent { n with Left = Some(child) } graph)
                else
                    descriptionsToGraph' rest (Map.add parent { n with Right = Some(child) } graph)
            | None ->
                if is_left = 1 then
                    descriptionsToGraph' rest (Map.add parent { Left = Some(child); Right = None } graph)
                else
                    descriptionsToGraph' rest (Map.add parent { Left = None; Right = Some(child) } graph)

    descriptionsToGraph' descriptions Map.empty

let findRoot (graph: Map<int, GraphNode>) : int =
    let parents = Map.keys graph |> Set.ofSeq

    let children =
        graph
        |> Map.fold
            (fun acc _ { Left = l; Right = r } ->
                match l, r with
                | None, None -> acc
                | Some (a), None -> Set.add a acc
                | None, Some (b) -> Set.add b acc
                | Some (a), Some (b) -> Set.add a acc |> Set.add b)
            Set.empty

    Set.difference parents children
    |> Set.toList
    |> List.head

let createBinaryTree (descriptions: (int * int * int) list) : BinaryTree =
    let rec createBinaryTree' root graph =
        match Map.tryFind root graph with
        | None -> TreeNode(root, TreeEnd, TreeEnd)
        | Some ({ Left = l; Right = r }) ->
            match l, r with
            | None, None -> TreeNode(root, TreeEnd, TreeEnd)
            | Some (a), None -> TreeNode(root, createBinaryTree' a graph, TreeEnd)
            | None, Some (b) -> TreeNode(root, TreeEnd, createBinaryTree' b graph)
            | Some (a), Some (b) -> TreeNode(root, createBinaryTree' a graph, createBinaryTree' b graph)

    let graph = descriptionsToGraph descriptions
    let root = findRoot graph
    createBinaryTree' root graph


let descriptions1 =
    [ (20, 15, 1)
      (20, 17, 0)
      (50, 20, 1)
      (50, 80, 0)
      (80, 19, 1) ]

createBinaryTree descriptions1

let descriptions2 = [ (1, 2, 1); (2, 3, 0); (3, 4, 1) ]
createBinaryTree descriptions2
