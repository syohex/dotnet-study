let closeStrings (word1: string) (word2: string) : bool =
    let composedCharacters (word: string) =
        let rec composedCharacters' cs acc =
            match cs with
            | [] -> acc
            | h :: t -> composedCharacters' t (Set.add h acc)

        composedCharacters' (word |> Seq.toList) Set.empty

    let charFrequences (word: string) =
        let rec charFrequences' cs acc =
            match cs with
            | [] -> acc
            | h :: t ->
                match Map.tryFind h acc with
                | Some (n) -> charFrequences' t (Map.add h (n + 1) acc)
                | None -> charFrequences' t (Map.add h 1 acc)

        charFrequences' (word |> Seq.toList) Map.empty

    let countFrequences (freq: Map<char, int>) =
        freq
        |> Map.values
        |> Seq.fold
            (fun acc n ->
                match Map.tryFind n acc with
                | Some (m) -> Map.add n (m + 1) acc
                | None -> Map.add n 1 acc)
            Map.empty

    let cs1, cs2 =
        composedCharacters word1, composedCharacters word2

    if cs1 <> cs2 then
        false
    else
        let count1 =
            word1 |> charFrequences |> countFrequences

        let count2 =
            word2 |> charFrequences |> countFrequences

        count1 = count2

// true
closeStrings "abc" "bca"

// false
closeStrings "a" "aa"

// true
closeStrings "cabbba" "abbccc"

// false
closeStrings "aab" "xxy"
