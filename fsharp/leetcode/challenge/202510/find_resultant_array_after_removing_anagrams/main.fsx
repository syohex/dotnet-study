let removeAnagrams (words: string list) : string list =
    let toFreq word =
        word
        |> Seq.fold
            (fun (acc: int[]) c ->
                let idx = int c - int 'a'
                acc.[idx] <- acc.[idx] + 1
                acc)
            (Array.zeroCreate 26)

    let rec removeAnagrams' words prev acc =
        match words with
        | [] -> List.rev acc
        | h :: t ->
            let freq = toFreq h

            if freq = prev then
                removeAnagrams' t prev acc
            else
                removeAnagrams' t freq (h :: acc)

    removeAnagrams' words [||] []

// ["abba","cd"]
removeAnagrams [ "abba"; "baba"; "bbaa"; "cd"; "cd" ]

// ["a","b","c","d","e"]
removeAnagrams [ "a"; "b"; "c"; "d"; "e" ]
