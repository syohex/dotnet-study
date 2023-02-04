let checkInclusion (s1: string) (s2: string) : bool =
    let toFreq (s: char list) : int [] =
        s
        |> List.fold
            (fun (acc: int []) c ->
                let index = int c - int 'a'
                acc.[index] <- acc.[index] + 1
                acc)
            (Array.zeroCreate 26)

    if s1.Length > s2.Length then
        false
    else
        let freq1 = toFreq (Seq.toList s1)

        s2
        |> Seq.toList
        |> List.windowed s1.Length
        |> List.map toFreq
        |> List.contains freq1

// true
checkInclusion "ab" "eidbaooo"

// false
checkInclusion "ab" "eidboaoo"
