let MOD: int64 = 1_000_000_007

let countOrders' (n: int64) : int64 =
    let rec countOrders'' (unpicked: int64) (undelivered: int64) cache : int64 * Map<(int64 * int64), int64> =
        if unpicked = 0 && undelivered = 0 then
            (1, cache)
        elif unpicked < 0
             || undelivered < 0
             || unpicked > undelivered then
            (0, cache)
        else
            match Map.tryFind (unpicked, undelivered) cache with
            | Some (v) -> (v, cache)
            | None ->
                let (ret1, cache) = countOrders'' (unpicked - 1L) undelivered cache
                let ret1 = (ret1 * unpicked) % MOD

                let (ret2, cache) = countOrders'' unpicked (undelivered - 1L) cache
                let picked = undelivered - unpicked
                let ret2 = (picked * ret2) % MOD

                let ret = (ret1 + ret2) % MOD
                (ret, Map.add (unpicked, undelivered) ret cache)

    let ret, _ = countOrders'' n n Map.empty
    ret

let countOrders (n: int) : int = int (countOrders' (int64 n))

// 1
countOrders 1

// 6
countOrders 2

// 90
countOrders 3

// 729647433
countOrders 8
