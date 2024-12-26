let findTargetSumWays (nums: int list) (target: int) : int =
    let rec findTargetSumWays' i nums target acc cache =
        match nums with
        | [] -> (if acc = target then 1 else 0), cache
        | h :: t ->
            match Map.tryFind (i, acc) cache with
            | Some(v) -> v, cache
            | None ->
                let ret1, cache = findTargetSumWays' (i + 1) t target (acc + h) cache
                let ret2, cache = findTargetSumWays' (i + 1) t target (acc - h) cache
                let ret = ret1 + ret2
                ret, Map.add (i, acc) ret cache

    findTargetSumWays' 0 nums target 0 Map.empty |> fst

// 5
findTargetSumWays [ 1; 1; 1; 1; 1 ] 3

// 1
findTargetSumWays [ 1 ] 1

// 7046
findTargetSumWays [ 0; 35; 32; 3; 4; 16; 12; 25; 47; 9; 14; 29; 7; 26; 17; 42; 21; 23; 48; 18 ] 20
