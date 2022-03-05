let numsToFrequences (nums: int list) : Map<int, int> =
    nums
    |> List.fold
        (fun acc n ->
            match Map.tryFind n acc with
            | None -> Map.add n 1 acc
            | Some (v) -> Map.add n (v + 1) acc)
        Map.empty

let deleteAndEarn (nums: int list) : int =
    let rec deleteAndEarn' i limit prevMax2 prevMax1 freq =
        if i > limit then
            prevMax1
        else
            let v = match Map.tryFind i freq with
                    | None -> 0
                    | Some(v) -> v

            let curMax = prevMax2 + (i * v)
            if curMax > prevMax1 then
                deleteAndEarn' (i + 1) limit prevMax1 curMax freq
            else
                deleteAndEarn' (i + 1) limit prevMax1 prevMax1 freq


    let freq = numsToFrequences nums
    let maxValue = Map.keys freq |> Seq.max

    let prevMax1 =
        match Map.tryFind 1 freq with
        | None -> 0
        | Some (v) -> v

    deleteAndEarn' 2 maxValue 0 prevMax1 freq

// 6
deleteAndEarn [ 3; 4; 2 ]

// 9
deleteAndEarn [ 2; 2; 3; 3; 3; 4 ]

// 4
deleteAndEarn [ 3; 1 ]
