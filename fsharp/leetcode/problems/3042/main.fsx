let countPrefixSuffixPairs (words: string list) : int =
    let rec countPrefixSuffixPairs' (words: string list) acc =
        match words with
        | [] -> acc
        | h :: t ->
            let matched =
                t |> List.filter (fun s -> s.StartsWith(h) && s.EndsWith(h)) |> List.length

            countPrefixSuffixPairs' t (acc + matched)

    countPrefixSuffixPairs' words 0

// 4
countPrefixSuffixPairs [ "a"; "aba"; "ababa"; "aa" ]

// 2
countPrefixSuffixPairs [ "pa"; "papa"; "ma"; "mama" ]

// 0
countPrefixSuffixPairs [ "abab"; "ab" ]
