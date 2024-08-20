let stoneGameII (piles: int list) : int =
    let rec stoneGameII' is_alice i m (piles: int[]) cache =
        if i >= piles.Length then
            0, cache
        else
            match Map.tryFind (is_alice, i, m) cache with
            | Some(v) -> v, cache
            | None ->
                let limit = min (2 * m) (piles.Length - i)
                let ret = if is_alice then -1 else 1_000_000_000

                let ret, _, cache =
                    seq { 1..limit }
                    |> Seq.fold
                        (fun (acc, score, cache) j ->
                            let score = score + piles.[i + j - 1]
                            let nextM = max j m

                            if is_alice then
                                let ret, cache = stoneGameII' false (i + j) nextM piles cache
                                (max acc (score + ret)), score, cache
                            else
                                let ret, cache = stoneGameII' true (i + j) nextM piles cache
                                (min acc ret), score, cache)
                        (ret, 0, cache)

                ret, Map.add (is_alice, i, m) ret cache

    stoneGameII' true 0 1 (List.toArray piles) Map.empty |> fst

// 10
stoneGameII [ 2; 7; 9; 4; 4 ]

// 104
stoneGameII [ 1; 2; 3; 4; 5; 100 ]
