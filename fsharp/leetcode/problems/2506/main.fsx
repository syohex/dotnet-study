let similarPairs (words: string list) : int =
    let rec similarPairs' s acc =
        match s with
        | [] -> acc
        | h :: t ->
            let similars = t |> List.filter (fun v -> h = v) |> List.length
            similarPairs' t (acc + similars)

    let s = words |> List.map Set.ofSeq
    similarPairs' s 0

// 2
similarPairs [ "aba"; "aabb"; "abcd"; "bac"; "aabc" ]

// 3
similarPairs [ "aabb"; "ab"; "ba" ]

// 0
similarPairs [ "nba"; "cba"; "dba" ]
