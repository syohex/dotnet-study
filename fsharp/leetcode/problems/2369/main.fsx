let validPartion (nums: int list) : bool =
    let rec validPartion' i (nums: int[]) cache =
        if i < 0 then
            true, cache
        else
            match Map.tryFind i cache with
            | Some(ret) -> ret, cache
            | None ->
                let ret1, cache1 =
                    if i >= 1 && nums.[i - 1] = nums.[i] then
                        validPartion' (i - 2) nums cache
                    else
                        false, cache

                let ret2, cache2 =
                    if i >= 2 && nums.[i - 2] = nums.[i - 1] && nums.[i - 1] = nums.[i] then
                        validPartion' (i - 3) nums cache1
                    else
                        false, cache1

                let ret3, cache3 =
                    if i >= 2 && nums.[i - 2] = nums.[i - 1] - 1 && nums.[i - 1] = nums.[i] - 1 then
                        validPartion' (i - 3) nums cache2
                    else
                        false, cache2

                let ret =
                    match ret1, ret2, ret3 with
                    | false, false, false -> false
                    | _ -> true

                ret, Map.add i ret cache3

    let nums' = Array.ofList nums
    validPartion' (nums'.Length - 1) nums' Map.empty |> fst

// true
validPartion [ 4; 4; 4; 5; 6 ]

// false
validPartion [ 1; 1; 1; 2 ]
