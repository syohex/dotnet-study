let longestPalindromeSubseq (s: string) : int =
    let rec longestPalindromeSubseq' left right (cs: char[]) cache =
        if left > right then
            0, cache
        elif left = right then
            1, cache
        else
            match Map.tryFind (left, right) cache with
            | Some(v) -> v, cache
            | None ->
                let ret, cache' =
                    if cs.[left] = cs.[right] then
                        let ret1, cache1 = longestPalindromeSubseq' (left + 1) (right - 1) cs cache
                        ret1 + 2, cache1
                    else
                        let ret1, cache1 = longestPalindromeSubseq' (left + 1) right cs cache
                        let ret2, cache2 = longestPalindromeSubseq' left (right - 1) cs cache1
                        System.Math.Max(ret1, ret2), cache2

                ret, Map.add (left, right) ret cache'

    let cs = s |> Seq.toArray
    longestPalindromeSubseq' 0 (cs.Length - 1) cs Map.empty |> fst

// 4
longestPalindromeSubseq "bbbab"

// 2
longestPalindromeSubseq "cbbd"
