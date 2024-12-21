let maxKDivisibleComponents (n: int) (edges: (int * int) list) (values: int[]) (k: int) : int =
    let rec edgesToGraph edges acc =
        match edges with
        | [] -> acc
        | (s, e) :: t ->
            let acc =
                match Map.tryFind e acc with
                | Some(v) -> Map.add e (s :: v) acc
                | None -> Map.add e [ s ] acc

            let acc =
                match Map.tryFind s acc with
                | Some(v) -> Map.add s (e :: v) acc
                | None -> Map.add s [ e ] acc

            edgesToGraph t acc

    let rec f node prev graph =
        match Map.tryFind node graph with
        | None -> 0, 0
        | Some(v) ->
            let sum, components =
                v
                |> List.fold
                    (fun (sum, components) next ->
                        if next = prev then
                            sum, components
                        else
                            let s, c = f next node graph
                            (sum + s) % k, components + c)
                    (values.[node], 0)

            sum, components + (if sum % k = 0 then 1 else 0)

    let graph = edgesToGraph edges Map.empty
    f 0 -1 graph |> snd

// 2
maxKDivisibleComponents 5 [ (0, 2); (1, 2); (1, 3); (2, 4) ] [| 1; 8; 1; 4; 4 |] 6

// 3
maxKDivisibleComponents 7 [ (0, 1); (0, 2); (1, 3); (1, 4); (2, 5); (2, 6) ] [| 3; 0; 6; 1; 5; 2; 1 |] 3
