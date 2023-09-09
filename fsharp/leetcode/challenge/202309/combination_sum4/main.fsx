let combinationSum4 (nums: int list) (target: int) : int =
    let rec combinationSum4' target nums cache =
        if target = 0 then
            1, cache
        else
            match Map.tryFind target cache with
            | Some(v) -> v, cache
            | None ->
                let ret, cache' =
                    nums
                    |> List.filter (fun num -> target - num >= 0)
                    |> List.fold
                        (fun (acc, cache) num ->
                            let ret, cache' = combinationSum4' (target - num) nums cache
                            acc + ret, cache')
                        (0, cache)

                ret, Map.add target ret cache'

    combinationSum4' target nums Map.empty |> fst

// 7
combinationSum4 [ 1; 2; 3 ] 4

// 0
combinationSum4 [ 9 ] 3

// 181997601
combinationSum4 [ 1; 2; 3 ] 32
