let wordSubsets (words1: string list) (words2: string list) : string list =
    let toFreq word =
        let rec toFreq' cs (acc: int []) =
            match cs with
            | [] -> acc
            | h :: t ->
                let index = int h - int 'a'
                acc.[index] <- acc.[index] + 1
                toFreq' t acc

        toFreq' (word |> Seq.toList) (Array.zeroCreate 26)

    let isSubset (a: int []) (b: int []) =
        seq { 0..25 }
        |> Seq.forall (fun i -> a.[i] >= b.[i])

    let words2Freq =
        words2
        |> List.map toFreq
        |> List.fold
            (fun (acc: int []) freq ->
                seq { 0..25 }
                |> Seq.iter (fun i -> acc.[i] <- System.Math.Max(acc.[i], freq.[i]))

                acc)
            (Array.zeroCreate 26)


    words1
    |> List.filter (fun word ->
        let freq = toFreq word
        isSubset freq words2Freq)

// ["facebook","google","leetcode"]
wordSubsets [ "amazon"
              "apple"
              "facebook"
              "google"
              "leetcode" ] [
    "e"
    "o"
]

// ["apple","google","leetcode"]
wordSubsets [ "amazon"
              "apple"
              "facebook"
              "google"
              "leetcode" ] [
    "l"
    "e"
]
