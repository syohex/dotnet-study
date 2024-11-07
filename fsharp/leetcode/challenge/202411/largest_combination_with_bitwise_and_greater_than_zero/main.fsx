let largestCombination (candidates: int list) : int =
    seq { 0..31 }
    |> Seq.fold
        (fun (acc: int[]) i ->
            let bit = 1 <<< i

            candidates
            |> List.iter (fun cand ->
                if cand &&& bit <> 0 then
                    acc.[i] <- acc.[i] + 1)

            acc)
        (Array.zeroCreate 31)
    |> Array.max

// 4
largestCombination [ 16; 17; 71; 62; 12; 24; 14 ]

// 2
largestCombination [ 8; 8 ]

// 28
largestCombination
    [ 84
      40
      66
      44
      91
      90
      1
      14
      73
      51
      47
      35
      18
      46
      18
      65
      55
      18
      16
      45
      43
      58
      90
      92
      91
      43
      44
      76
      85
      72
      24
      89
      60
      94
      81
      90
      86
      79
      84
      41
      41
      28
      44 ]
