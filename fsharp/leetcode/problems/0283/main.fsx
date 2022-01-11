let moveZeros (ns: int list) : int list =
    let rec addZero n ns =
        if n = 0 then
            ns
        else
            0 :: addZero (n - 1) ns

    let rec moveZeros' ns count acc =
        match ns with
        | [] -> addZero count acc |> List.rev
        | h :: t ->
            if h <> 0 then
                moveZeros' t count (h :: acc)
            else
                moveZeros' t (count + 1) acc

    moveZeros' ns 0 []


moveZeros [ 0; 1; 0; 3; 12 ]
moveZeros [ 0 ]
