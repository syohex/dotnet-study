let minSteps (n: int) : int =
    let rec minSteps' len pasteSize n cache =
        if len = n then
            0, cache
        elif len > n then
            1001, cache
        else
            match Map.tryFind (len, pasteSize) cache with
            | Some(v) -> v, cache
            | None ->
                let ret1, cache = minSteps' (len + pasteSize) pasteSize n cache
                let ret2, cache = minSteps' (len * 2) len n cache
                let ret = min (ret1 + 1) (ret2 + 2)
                ret, Map.add (len, pasteSize) ret cache

    if n = 1 then 0 else 1 + (minSteps' 1 1 n Map.empty |> fst)

// 3
minSteps 3

// 0
minSteps 1
