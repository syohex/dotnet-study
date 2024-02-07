let frequencySort (s: string) : string =
    s
    |> Seq.fold
        (fun acc c ->
            match Map.tryFind c acc with
            | Some(v) -> Map.add c (v + 1) acc
            | None -> Map.add c 1 acc)
        Map.empty
    |> Map.toList
    |> List.sortWith (fun (c1, n1) (c2, n2) -> if n1 = n2 then compare c1 c2 else compare n2 n1)
    |> List.fold (fun acc (c, v) -> seq { 1..v } |> Seq.fold (fun acc _ -> acc + string c) acc) ""

// "eert"
frequencySort "tree"

// "aaaccc"
frequencySort "cccaaa"

// "bbAa"
frequencySort "Aabb"
