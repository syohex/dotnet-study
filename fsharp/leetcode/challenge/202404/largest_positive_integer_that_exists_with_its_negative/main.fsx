let findMaxK (nums: int list) : int =
    let s = Set.ofList nums

    let cands =
        nums |> List.filter (fun n -> n > 0) |> List.filter (fun n -> Set.contains -n s)

    match cands with
    | [] -> -1
    | _ -> List.max cands

// 3
findMaxK [ -1; 2; 3; -3 ]

// 7
findMaxK [ -1; 10; 6; 7; -7; 1 ]

// -1
findMaxK [ -10; 8; 6; 7; -2; -3 ]
