let knightDialer (n: int) : int =
    let modulo = 1_000_000_007

    let moves =
        [| [ 4; 6 ]
           [ 6; 8 ]
           [ 7; 9 ]
           [ 4; 8 ]
           [ 3; 9; 0 ]
           []
           [ 1; 7; 0 ]
           [ 2; 6 ]
           [ 1; 3 ]
           [ 2; 4 ] |]

    let rec knightDialer' n pos cache =
        if n = 1 then
            1, cache
        else
            match Map.tryFind (n, pos) cache with
            | Some(v) -> v, cache
            | None ->
                let ret, cache' =
                    moves.[pos]
                    |> List.fold
                        (fun (acc, cache) next ->
                            let ret, cache' = knightDialer' (n - 1) next cache
                            (acc + ret) % modulo, cache')
                        (0, cache)

                ret, Map.add (n, pos) ret cache'

    seq { 0..9 }
    |> Seq.fold
        (fun (acc, cache) pos ->
            let ret, cache' = knightDialer' n pos cache
            (acc + ret) % modulo, cache')
        (0, Map.empty)
    |> fst

// 10
knightDialer 1

// 20
knightDialer 2

// 136006598
knightDialer 3131
