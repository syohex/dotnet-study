let minimumPushes (word: string) : int =
    word
    |> Seq.countBy id
    |> Seq.map snd
    |> Seq.sort
    |> Seq.rev
    |> Seq.fold
        (fun (acc, (ones, twos, threes)) n ->
            if ones > 0 then acc + n, (ones - 1, twos, threes)
            elif twos > 0 then acc + 2 * n, (0, twos - 1, threes)
            elif threes > 0 then acc + 3 * n, (0, 0, threes - 1)
            else acc + 4 * n, (0, 0, 0))
        (0, (8, 8, 8))
    |> fst

// 5
minimumPushes "abcde"

// 12
minimumPushes "xyzxyzxyzxyz"

// 24
minimumPushes "aabbccddeeffgghhiiiiii"

// 52
minimumPushes "abcdefghijklmnopqrstuvwxy"
