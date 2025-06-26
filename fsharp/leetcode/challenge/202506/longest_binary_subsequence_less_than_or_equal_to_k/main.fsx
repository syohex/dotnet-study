let longestSubsequence (s: string) (k: int) : int =
    let rec longestSubsequence' cs k sum acc =
        match cs with
        | [] -> acc
        | (i, h) :: t ->
            if h = '1' then
                if i <= 30 then
                    let v = sum + (1 <<< i)

                    if v <= k then
                        longestSubsequence' t k v (acc + 1)
                    else
                        longestSubsequence' t k sum acc
                else
                    longestSubsequence' t k sum acc
            else
                longestSubsequence' t k sum (acc + 1)

    let cs = s |> Seq.toList |> List.rev |> List.indexed
    longestSubsequence' cs k 0 0

// 5
longestSubsequence "1001010" 5

// 6
longestSubsequence "00101001" 1
