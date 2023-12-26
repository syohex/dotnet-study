let numRollsToTarget (n: int) (k: int) (target: int) : int =
    let rec numRollsToTarget' n (k: int) target sum cache =
        if n = 0 then
            if sum = target then 1, cache else 0, cache
        else
            match Map.tryFind (n, sum) cache with
            | Some(v) -> v, cache
            | None ->
                let limit = System.Math.Min(k, target - sum)

                let ret, cache' =
                    seq { 1..limit }
                    |> Seq.fold
                        (fun (acc, cache) i ->
                            let ret, cache' = numRollsToTarget' (n - 1) k target (sum + i) cache
                            (acc + ret) % 1_000_000_007, cache')
                        (0, cache)

                ret, Map.add (n, sum) ret cache'

    numRollsToTarget' n k target 0 Map.empty |> fst

// 1
numRollsToTarget 1 6 3

// 6
numRollsToTarget 2 6 7

// 222616187
numRollsToTarget 30 30 500
