let minimumDeletions (s: string) : int =
    let rec minimumDeletions' cs bCount acc =
        match cs with
        | [] -> acc
        | (h, aCount) :: t ->
            let acc = min acc (aCount + bCount)
            let bCount = if h = 'b' then bCount + 1 else bCount
            minimumDeletions' t bCount acc

    let cs = s |> Seq.toList
    let aPostfixes = cs
                     |> List.rev
                     |> List.fold (fun (acc, count) c ->
                                    if c = 'a' then
                                        count :: acc, count + 1
                                    else
                                        count :: acc, count) ([], 0)
                      |> fst

    minimumDeletions' (List.zip cs aPostfixes) 0 System.Int32.MaxValue

// 2
minimumDeletions "aababbab"

// 2
minimumDeletions "bbaaaaabb"
