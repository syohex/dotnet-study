let findRelativeRanks (score: int list) : string list =
    let m =
        score
        |> List.sort
        |> List.rev
        |> List.mapi (fun i s ->
            match i with
            | 0 -> s, "Gold Medal"
            | 1 -> s, "Silver Medal"
            | 2 -> s, "Bronze Medal"
            | n -> s, string (n + 1))
        |> Map.ofList

    score |> List.map (fun s -> Map.find s m)

// ["Gold Medal","Silver Medal","Bronze Medal","4","5"]
findRelativeRanks [ 5; 4; 3; 2; 1 ]

// ["Gold Medal","5","Bronze Medal","Silver Medal","4"]
findRelativeRanks [ 10; 3; 8; 9; 4 ]
