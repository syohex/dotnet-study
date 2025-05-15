let getLongestSubsequence (words: string list) (groups: int list) : string list =
    let rec getLongestSubsequence' v prev acc =
        match v with
        | [] -> acc |> List.rev
        | (word, group) :: t ->
            if group = prev then
                getLongestSubsequence' t group acc
            else
                getLongestSubsequence' t group (word :: acc)

    getLongestSubsequence' (List.zip words groups) -1 []

// ["e", "b"]
getLongestSubsequence [ "e"; "a"; "b" ] [ 0; 0; 1 ]

// ["a", "b", "c"]
getLongestSubsequence [ "a"; "b"; "c"; "d" ] [ 1; 0; 1; 1 ]
