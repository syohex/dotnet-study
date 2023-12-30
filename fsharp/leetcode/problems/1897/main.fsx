let makeEqual (words: string list) : bool =
    let len = List.length words

    words
    |> List.fold
        (fun acc word ->
            word
            |> Seq.fold
                (fun acc c ->
                    match Map.tryFind c acc with
                    | Some(v) -> Map.add c (v + 1) acc
                    | None -> Map.add c 1 acc)
                acc)
        Map.empty
    |> Map.forall (fun _ v -> v % len = 0)

// true
makeEqual [ "abc"; "aabc"; "bc" ]

// false
makeEqual [ "ab"; "a" ]
