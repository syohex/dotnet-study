let findLonely (nums: int list) : int list =
    let rec toFreq nums m =
        match nums with
        | [] -> m
        | head :: tail ->
            match Map.tryFind head m with
            | Some (v) -> toFreq tail (Map.add head (v + 1) m)
            | None -> toFreq tail (Map.add head 1 m)

    let freq = toFreq nums Map.empty

    freq
    |> Map.fold
        (fun acc k v ->
            match v, Map.tryFind (k - 1) freq, Map.tryFind (k + 1) freq with
            | 1, None, None -> k :: acc
            | _, _, _ -> acc)
        []

// [8, 10]
findLonely [ 10; 6; 5; 8 ] |> List.sort

// [1, 5]
findLonely [ 1; 3; 5; 3 ] |> List.sort
