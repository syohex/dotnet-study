let findOriginalArray (changed: int list) : int list =
    let rec findOriginalArray' changed m len acc =
        match changed with
        | [] ->
            if (List.length acc * 2) = len then
                acc |> List.rev
            else
                []
        | h :: t ->
            let count = Map.find h m

            if count = 0 then
                findOriginalArray' t m len acc
            else
                let m' = Map.add h (count - 1) m
                let doubled = h * 2

                match Map.tryFind doubled m' with
                | None -> []
                | Some (v) ->
                    if v = 0 then
                        []
                    else
                        let m'' = Map.add doubled (v - 1) m'
                        findOriginalArray' t m'' len (h :: acc)

    let len = List.length changed

    let m =
        changed
        |> List.fold
            (fun acc n ->
                match Map.tryFind n acc with
                | None -> Map.add n 1 acc
                | Some (v) -> Map.add n (v + 1) acc)
            Map.empty

    findOriginalArray' (changed |> List.sort) m len []

// [1;3;4]
findOriginalArray [ 1; 3; 4; 2; 6; 8 ]

// []
findOriginalArray [ 6; 3; 0; 1 ]

// []
findOriginalArray [ 1 ]

// []
findOriginalArray [ 0 ]

// [0]
findOriginalArray [ 0; 0 ]

// []
findOriginalArray [ 5
                    7
                    2
                    10
                    4
                    2
                    7
                    14 ]
