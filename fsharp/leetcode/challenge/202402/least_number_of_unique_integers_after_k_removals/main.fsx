let toFreq arr =
    let rec toFreq' arr acc =
        match arr with
        | [] -> acc
        | h :: t ->
            match Map.tryFind h acc with
            | Some(v) -> toFreq' t (Map.add h (v + 1) acc)
            | None -> toFreq' t (Map.add h 1 acc)

    toFreq' arr Map.empty

let findLeastNumOfUniqueInts (arr: int list) (k: int) =
    let rec findLeastNumOfUniqueInts' freq k =
        match freq with
        | [] -> 0
        | (_, count) :: t ->
            if count > k then
                List.length freq
            else
                findLeastNumOfUniqueInts' t (k - count)

    let freq =
        toFreq arr |> Map.toList |> List.sortWith (fun (_, v1) (_, v2) -> compare v1 v2)

    findLeastNumOfUniqueInts' freq k

// 1
findLeastNumOfUniqueInts [ 5; 5; 4 ] 1

// 2
findLeastNumOfUniqueInts [ 4; 3; 1; 1; 3; 3; 2 ] 2
