let equalSubstring (s: string) (t: string) (maxCost: int) : int =
    let rec adjustCost cost left maxCost (s: char[]) (t: char[]) =
        if cost <= maxCost then
            cost, left
        else
            let cost' = cost - ((int s.[left] - int t.[left]) |> abs)
            adjustCost cost' (left + 1) maxCost s t

    let rec equalSubstring' i (s: char[]) (t: char[]) left maxCost cost ret =
        if i >= s.Length then
            ret
        else
            let cost' = cost + ((int s.[i] - int t.[i]) |> abs)
            let cost'', left' = adjustCost cost' left maxCost s t
            equalSubstring' (i + 1) s t left' maxCost cost'' (max ret (i - left' + 1))

    equalSubstring' 0 (s |> Seq.toArray) (t |> Seq.toArray) 0 maxCost 0 0

// 3
equalSubstring "abcd" "bcdf" 3

// 1
equalSubstring "abcd" "cdef" 3

// 1
equalSubstring "abcd" "acde" 0
