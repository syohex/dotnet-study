let countPalindromicSubsequence (s: string) : int =
    let v =
        s
        |> Seq.indexed
        |> Seq.fold
            (fun (acc: (int * int)[]) (i, c) ->
                let idx = int c - int 'a'

                match acc.[idx] with
                | (-1, _) -> acc.[idx] <- i, -1
                | (h, _) -> acc.[idx] <- h, i

                acc)
            (Array.init 26 (fun _ -> -1, -1))

    v
    |> Array.fold
        (fun acc (f, t) ->
            match f, t with
            | -1, _
            | _, -1 -> acc
            | f, t ->
                let uniques = s |> Seq.take t |> Seq.skip (f + 1) |> Set.ofSeq |> Set.count
                acc + uniques)
        0

// 3
countPalindromicSubsequence "aabca"

// 0
countPalindromicSubsequence "adc"

// 4
countPalindromicSubsequence "bbcbaba"
