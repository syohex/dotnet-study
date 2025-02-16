let constructDistancedSequence (n: int) : int[] =
    let rec f pos (acc: int[]) used : bool * int[] =
        if pos >= acc.Length then
            Set.count used = n, acc
        elif acc.[pos] <> 0 then
            f (pos + 1) acc used
        else
            seq { 1..n }
            |> Seq.rev
            |> Seq.fold
                (fun (ok, (acc: int[])) i ->
                    if ok then
                        true, acc
                    elif Set.contains i used then
                        false, acc
                    else
                        let pos2 = if i = 1 then pos else pos + i

                        if pos2 >= acc.Length || acc.[pos2] <> 0 then
                            false, acc
                        else
                            acc.[pos] <- i
                            acc.[pos2] <- i
                            let ok2, acc2 = f (pos + 1) acc (Set.add i used)

                            if ok2 then
                                ok2, acc2
                            else
                                acc.[pos] <- 0
                                acc.[pos2] <- 0
                                false, acc2)
                (false, acc)

    let acc = Array.zeroCreate (2 * n - 1)
    f 0 acc Set.empty |> snd

// [3,1,2,3,2]
constructDistancedSequence 3

// [5,3,1,4,3,5,2,4,2]
constructDistancedSequence 5
