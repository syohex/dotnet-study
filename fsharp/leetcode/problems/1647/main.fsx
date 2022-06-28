let minDeletions (s: string) : int =
    let rec findNextEmpty count freq acc =
        if count = 0 then
            acc, freq
        else if Set.contains count freq then
            findNextEmpty (count - 1) freq (acc + 1)
        else
            acc, Set.add count freq

    let rec minDeletions' i (table: int []) freq acc =
        if i >= 26 then
            acc
        else
            let deletes, freq' = findNextEmpty table.[i] freq 0
            minDeletions' (i + 1) table freq' (acc + deletes)

    let table =
        s
        |> Seq.fold
            (fun (acc: int []) c ->
                let index = (int c) - (int 'a')
                acc.[index] <- acc.[index] + 1
                acc)
            (Array.zeroCreate 26)

    minDeletions' 0 table Set.empty 0

// 0
minDeletions "aab"

// 2
minDeletions "aaabbbcc"

// 2
minDeletions "ceabaacb"

// 1
minDeletions "accdcdadddbaadbc"

// 2
minDeletions "bbcebab"
