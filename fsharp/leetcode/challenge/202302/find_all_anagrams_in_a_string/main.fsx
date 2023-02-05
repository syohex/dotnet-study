let toFreq (cs: char list) : int array =
    cs
    |> List.fold
        (fun acc c ->
            let index = int c - int 'a'
            acc.[index] <- acc.[index] + 1
            acc)
        (Array.zeroCreate 26)

let findAnagrams (s: string) (p: string) : int list =
    if p.Length > s.Length then
        []
    else
        let pFreq = toFreq (p |> List.ofSeq)

        s
        |> List.ofSeq
        |> List.windowed p.Length
        |> List.map toFreq
        |> List.indexed
        |> List.filter (fun (i, window) -> pFreq = window)
        |> List.map fst

// [0, 6]
findAnagrams "cbaebabacd" "abc"

// [0, 1, 2]
findAnagrams "abab" "ab"

// []
findAnagrams "a" "abcde"
