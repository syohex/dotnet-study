let findCommonResponse (responses: string list list) : string =
    responses
    |> List.map Set.ofList
    |> List.fold
        (fun acc v ->
            v
            |> Set.fold
                (fun acc' word ->
                    match Map.tryFind word acc' with
                    | Some(v) -> Map.add word (v + 1) acc'
                    | None -> Map.add word 1 acc')
                acc)
        Map.empty
    |> Map.fold
        (fun (ret, maxCount) k v ->
            if v > maxCount || (v = maxCount && k < ret) then
                k, v
            else
                ret, maxCount)
        ("", 0)
    |> fst

// "good"
findCommonResponse
    [ [ "good"; "ok"; "good"; "ok" ]
      [ "ok"; "bad"; "good"; "ok"; "ok" ]
      [ "good" ]
      [ "bad" ] ]

// "bad"
findCommonResponse
    [ [ "good"; "ok"; "good" ]
      [ "ok"; "bad" ]
      [ "bad"; "notsure" ]
      [ "great"; "good" ] ]
