let minFlipsMonoInc (s: string) : int =
    let nums = s |> Seq.map (fun c -> int c - int '0') |> Seq.toList

    let ones =
        nums
        |> List.fold
            (fun (acc, prev) n ->
                let prev' = prev + if n = 1 then 1 else 0
                prev' :: acc, prev')
            ([ 0 ], 0)
        |> fst
        |> List.rev

    let zeros =
        nums
        |> List.rev
        |> List.fold
            (fun (acc, prev) n ->
                let prev' = prev + if n = 0 then 1 else 0
                prev' :: acc, prev')
            ([ 0 ], 0)
        |> fst

    ones
    |> List.zip zeros
    |> List.fold (fun ret (one, zero) -> System.Math.Min(ret, one + zero)) System.Int32.MaxValue

// 1
minFlipsMonoInc "00110"

// 2
minFlipsMonoInc "010110"

// 2
minFlipsMonoInc "00011000"

// 5
minFlipsMonoInc "10011111110010111011"
