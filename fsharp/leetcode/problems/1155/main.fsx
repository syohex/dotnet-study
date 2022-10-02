let numRollsToTarget (n: int) (k: int) (target: int) : int =
    let MOD = 1_000_000_007

    let rec numRollsToTarget i sum n (k: int) target cache =
        if i = n then
            if sum = target then
                1, cache
            else
                0, cache
        else
            match Map.tryFind (i, sum) cache with
            | Some (v) -> v, cache
            | None ->
                let max = System.Math.Min(k, target - sum)

                let ret, cache' =
                    seq { 1 .. max }
                    |> Seq.fold
                        (fun (ret, cache') j ->
                            let v, cache'' =
                                numRollsToTarget (i + 1) (sum + j) n k target cache'

                            (ret + v) % MOD, cache'')
                        (0, cache)

                ret, Map.add (i, sum) ret cache'

    numRollsToTarget 0 0 n k target Map.empty |> fst

// 1
numRollsToTarget 1 6 3

// 6
numRollsToTarget 2 6 7

// 222616187
numRollsToTarget 30 30 500