let validateBinaryTreeNodes (n: int) (leftChild: int list) (rightChild: int list) : bool =
    let rec validateBinaryTreeNodes' q visited (leftChild: int[]) (rightChild: int[]) =
        match q with
        | [] -> Set.count visited = n
        | h :: t ->
            let q', visited', ok =
                [ leftChild.[h]; rightChild.[h] ]
                |> List.filter ((<>) -1)
                |> List.fold
                    (fun (acc, visited, ok) node ->
                        if ok then
                            if Set.contains node visited then
                                acc, visited, false
                            else
                                node :: acc, Set.add node visited, true
                        else
                            acc, visited, false)
                    (t, visited, true)

            if ok then
                validateBinaryTreeNodes' q' visited' leftChild rightChild
            else
                false


    let children = Set.ofList (leftChild @ rightChild)

    let root =
        seq { 0 .. (n - 1) } |> Seq.tryFind (fun i -> not <| Set.contains i children)

    match root with
    | None -> false
    | Some(root) ->
        let visited = Set.empty |> Set.add root
        validateBinaryTreeNodes' [ root ] visited (List.toArray leftChild) (List.toArray rightChild)

// true
validateBinaryTreeNodes 4 [ 1; -1; 3; -1 ] [ 2; -1; -1; -1 ]

// false
validateBinaryTreeNodes 4 [ 1; -1; 3; -1 ] [ 2; 3; -1; -1 ]

// false
validateBinaryTreeNodes 2 [ 1 ] [ 0 ]

// true
validateBinaryTreeNodes 4 [ 3; -1; 1; -1 ] [ -1; -1; 0; -1 ]
