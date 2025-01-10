let wordSubsets (words1: string list) (words2: string list) : string list =
    let toFreq (s: string) : int[] =
        s
        |> Seq.fold
            (fun (acc: int[]) c ->
                let index = int c - int 'a'
                acc.[index] <- acc.[index] + 1
                acc)
            (Array.zeroCreate 26)

    let rec mergeFreq words acc =
        match words with
        | [] -> acc
        | h :: t ->
            let freq = toFreq h
            let acc = Array.zip acc freq |> Array.map (fun (a, b) -> max a b)
            mergeFreq t acc

    let isSubset (a: int[]) (b: int[]) =
        seq { 0..25 } |> Seq.forall (fun i -> a.[i] >= b.[i])

    let merged = mergeFreq words2 (Array.zeroCreate 26)

    words1
    |> List.filter (fun w ->
        let freq = toFreq w
        isSubset freq merged)

// ["facebook";"google";"leetcode"]
wordSubsets [ "amazon"; "apple"; "facebook"; "google"; "leetcode" ] [ "e"; "o" ]

// ["apple";"google";"leetcode"]
wordSubsets [ "amazon"; "apple"; "facebook"; "google"; "leetcode" ] [ "l"; "e" ]
