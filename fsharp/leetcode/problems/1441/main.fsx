let buildArray (target: int list) (n: int) : string list =
    target
    |> List.fold
        (fun (acc, i) num ->
            let diff = num - i - 1

            let acc' =
                seq { 0 .. (diff - 1) } |> Seq.fold (fun acc _ -> "Pop" :: "Push" :: acc) acc

            "Push" :: acc', i + 1 + diff)
        ([], 0)
    |> fst
    |> List.rev

// ["Push", "Push", "Pop", "Push"]
buildArray [ 1; 3 ] 3

// ["Push", "Push", "Push"]
buildArray [ 1; 2; 3 ] 3

// ["Push", "Push"]
buildArray [ 1; 2 ] 4

// ["Push", "Pop", "Push", "Push", "Push"]
buildArray [ 2; 3; 4 ] 4
