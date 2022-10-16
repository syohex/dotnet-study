let findMaxK (nums: int list) : int =
    let s = Set.ofList nums

    let cands =
        s
        |> Set.filter (fun n -> n > 0 && Set.contains -n s)

    if Set.isEmpty cands then
        -1
    else
        cands |> Set.maxElement

// 3
findMaxK [ -1; 2; -3; 3 ]

// 7
findMaxK [ -1; 10; 6; 7; -7; 1 ]

// -1
findMaxK [ -10; 8; 6; 7; -2; -3 ]
