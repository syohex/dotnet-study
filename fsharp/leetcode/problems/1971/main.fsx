let validPath (n: int) (edges: (int * int) list) (source: int) (destination: int) : bool =
    let rec validPath' q graph destination visited =
        match q with
        | [] -> false
        | h :: t ->
            if h = destination then
                true
            else
                let visited' = Set.add h visited

                match Map.tryFind h graph with
                | None -> validPath' t graph destination visited'
                | Some(v) ->
                    let q' =
                        v
                        |> List.fold (fun acc node -> if Set.contains node visited' then acc else node :: acc) t

                    validPath' q' graph destination visited'

    let graph =
        edges
        |> List.fold
            (fun acc (s, d) ->
                let acc' =
                    match Map.tryFind s acc with
                    | None -> Map.add s [ d ] acc
                    | Some(v) -> Map.add s (d :: v) acc

                match Map.tryFind d acc' with
                | None -> Map.add d [ s ] acc'
                | Some(v2) -> Map.add d (s :: v2) acc')
            Map.empty

    validPath' [ source ] graph destination Set.empty

// true
validPath 3 [ (0, 1); (1, 2); (2, 0) ] 0 2

// false
validPath 6 [ (0, 1); (0, 2); (3, 5); (5, 4); (4, 3) ] 0 5
