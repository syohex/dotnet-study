let countMaxOrSubsets (nums: int list) : int =
    let rec countMaxOrSubsets' i nums acc maxOr cache =
        match nums with
        | [] -> (if acc = maxOr then 1 else 0), cache
        | h :: t ->
            let key = i, acc

            match Map.tryFind key cache with
            | Some(v) -> v, cache
            | None ->
                let ret1, cache = countMaxOrSubsets' (i + 1) t acc maxOr cache
                let ret2, cache = countMaxOrSubsets' (i + 1) t (acc ||| h) maxOr cache
                let ret = ret1 + ret2
                ret, Map.add key ret cache

    let maxOr = List.reduce (fun acc n -> acc ||| n) nums
    countMaxOrSubsets' 0 nums 0 maxOr Map.empty |> fst

// 2
countMaxOrSubsets [ 3; 1 ]

// 7
countMaxOrSubsets [ 2; 2; 2 ]

// 6
countMaxOrSubsets [ 3; 2; 1; 5]
