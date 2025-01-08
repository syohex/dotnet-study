let countPrefixSuffixPairs (words: string list) : int =
    let isPrefixAndSuffix (str1: string) (str2: string) : bool =
        str2.StartsWith(str1) && str2.EndsWith(str1)

    let rec countPrefixSuffixPairs' words acc =
        match words with
        | [] -> acc
        | h :: t ->
            let n = List.filter (fun s -> isPrefixAndSuffix h s) t |> List.length
            countPrefixSuffixPairs' t (acc + n)

    countPrefixSuffixPairs' words 0

// 4
countPrefixSuffixPairs [ "a"; "aba"; "ababa"; "aa" ]

// 2
countPrefixSuffixPairs [ "pa"; "papa"; "ma"; "mama" ]

// 0
countPrefixSuffixPairs [ "abab"; "ab" ]
