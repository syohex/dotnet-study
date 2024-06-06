let isNStraightHand (hand: int list) (groupSize: int) : bool =
    let rec isNStraightHand' freq groupSize =
        if Map.isEmpty freq then
            true
        else
            let minKey = freq |> Map.keys |> Seq.min

            let freq', ok =
                seq { minKey .. (minKey + groupSize - 1) }
                |> Seq.fold
                    (fun (acc, ok) n ->
                        if not ok then
                            acc, false
                        else
                            match Map.tryFind n acc with
                            | Some(v) ->
                                if v = 1 then
                                    Map.remove n acc, ok
                                else
                                    Map.add n (v - 1) acc, ok
                            | None -> acc, false)
                    (freq, true)

            if ok then isNStraightHand' freq' groupSize else false

    if (List.length hand) % groupSize <> 0 then
        false
    else
        let freq =
            hand
            |> List.fold
                (fun acc n ->
                    match Map.tryFind n acc with
                    | None -> Map.add n 1 acc
                    | Some(v) -> Map.add n (v + 1) acc)
                Map.empty

        isNStraightHand' freq groupSize

// true
isNStraightHand [ 1; 2; 3; 6; 2; 3; 4; 7; 8 ] 3

// false
isNStraightHand [ 1; 2; 3; 4; 5 ] 4
