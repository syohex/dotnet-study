let minMutation (start: string) (last: string) (bank: string list) : int =
    let isOneMutation (a: string) (b: string) : bool =
        (seq { 0..7 }
         |> Seq.filter (fun i -> a.[i] = b.[i])
         |> Seq.length) = 7


    let rec minMutation' gene target steps bank visited =
        if gene = target then
            steps
        else
            let candidates =
                bank
                |> List.filter (fun b ->
                    (Set.contains b visited |> not)
                    && isOneMutation gene b)

            candidates
            |> List.fold
                (fun acc cand ->
                    let acc' = minMutation' cand target (steps + 1) bank (Set.add cand visited)
                    System.Math.Min(acc, acc'))
                System.Int32.MaxValue

    let minSteps = minMutation' start last 0 bank (Set.empty |> Set.add start)

    if minSteps = System.Int32.MaxValue then
        -1
    else
        minSteps

// 1
minMutation "AACCGGTT" "AACCGGTA" [ "AACCGGTA" ]

// 2
minMutation "AACCGGTT" "AAACGGTA" [ "AACCGGTA"; "AACCGCTA"; "AAACGGTA" ]

// 3
minMutation "AAAAACCC" "AACCCCCC" [ "AAAACCCC"; "AAACCCCC"; "AACCCCCC" ]
