let closeStrings (word1: string) (word2: string) : bool =
    let toFreq w =
        let rec toFreq' cs acc =
            match cs with
            | [] -> acc
            | h :: t ->
                match Map.tryFind h acc with
                | None -> toFreq' t (Map.add h 1 acc)
                | Some(v) -> toFreq' t (Map.add h (v + 1) acc)

        toFreq' (Seq.toList w) Map.empty

    if word1.Length <> word2.Length then
        false
    else
        let f1, f2 = toFreq word1, toFreq word2
        let sortedValues = Map.values >> Seq.sort >> Seq.toList
        let containChars = Map.keys >> Set.ofSeq
        let c1, c2 = sortedValues f1, sortedValues f2
        let a1, a2 = containChars f1, containChars f2
        c1 = c2 && a1 = a2

// true
closeStrings "abc" "bca"

// false
closeStrings "a" "aa"

// true
closeStrings "cabbba" "abbccc"

// false
closeStrings "aba" "cdc"
