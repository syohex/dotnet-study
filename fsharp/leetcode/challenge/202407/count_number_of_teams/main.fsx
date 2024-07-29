let numTeams (rating: int list) : int =
    let rec numTeams' i (rating: int[]) count cmp cache =
        if count = 3 then
            1, cache
        elif i >= rating.Length then
            0, cache
        else
            match Map.tryFind (i, count) cache with
            | Some(v) -> v, cache
            | None ->
                let ret, cache =
                    seq { (i + 1) .. (rating.Length - 1) }
                    |> Seq.fold
                        (fun (acc, cache) j ->
                            if cmp rating.[i] rating.[j] then
                                let ret, cache = numTeams' j rating (count + 1) cmp cache
                                acc + ret, cache
                            else
                                acc, cache)
                        (0, cache)

                ret, (Map.add (i, count) ret cache)

    let rating = Array.ofList rating

    seq { 0 .. (rating.Length - 1) }
    |> Seq.fold
        (fun (acc, (cache1, cache2)) i ->
            let ret1, cache1 = numTeams' i rating 1 (<) cache1
            let ret2, cache2 = numTeams' i rating 1 (>) cache2
            acc + ret1 + ret2, (cache1, cache2))
        (0, (Map.empty, Map.empty))
    |> fst

// 3
numTeams [ 2; 5; 3; 4; 1 ]

// 0
numTeams [ 2; 1; 3 ]

// 4
numTeams [ 1; 2; 3; 4 ]
