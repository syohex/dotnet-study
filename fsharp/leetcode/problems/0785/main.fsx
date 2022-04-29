type Color =
    | None
    | Red
    | Blue
    | Invalid

let paintNodes (index: int) (graph: int list []) (colors: Color []) =
    let rec paintNodes' nodes (graph: int list []) (colors: Color []) =
        match nodes with
        | [] -> true
        | h :: t ->
            let adjacents = graph.[h]

            let nodes' =
                adjacents
                |> List.fold
                    (fun acc n ->
                        match colors.[n] with
                        | None ->
                            colors.[n] <- if colors.[h] = Red then Blue else Red
                            n :: acc
                        | c ->
                            if c = colors.[h] then
                                colors.[n] <- Invalid

                            acc)
                    t

            if Array.tryFind ((=) Invalid) colors
               |> Option.isSome then
                false
            else
                paintNodes' nodes' graph colors

    paintNodes' [ index ] graph colors


let isBipartite (graph: int list []) : bool =
    let rec isBipartite' i limit (graph: int list []) (colors: Color []) =
        if i = limit then
            true
        else if paintNodes i graph colors then
            isBipartite' (i + 1) limit graph colors
        else
            false

    let limit = graph.Length
    let colors = Array.init limit (fun _ -> Color.None)
    isBipartite' 0 limit graph colors

// false
isBipartite [| [ 1; 2; 3 ]
               [ 0; 2 ]
               [ 0; 1; 3 ]
               [ 0; 2 ] |]

// true
isBipartite [| [ 1; 3 ]
               [ 0; 2 ]
               [ 1; 3 ]
               [ 0; 2 ] |]
