let findStartAndLastPosition (c: char) (s: string) : int * int =
    s
    |> Seq.indexed
    |> Seq.fold
        (fun (first, last) (i, d) ->
            if c = d then
                if first = -1 then i, last else first, i
            else
                first, last)
        (-1, -1)

let countPalindromicSubsequence (s: string) : int =
    s
    |> Set.ofSeq
    |> Set.map (fun c -> findStartAndLastPosition c s)
    |> Set.filter (fun (_, last) -> last <> -1)
    |> Set.fold
        (fun acc (first, last) ->
            let uniqueChars = s.Substring(first + 1, last - first - 1) |> Set.ofSeq |> Set.count
            acc + uniqueChars)
        0

// 3
countPalindromicSubsequence "aabca"

// 0
countPalindromicSubsequence "adc"

// 4
countPalindromicSubsequence "bbcbaba"
