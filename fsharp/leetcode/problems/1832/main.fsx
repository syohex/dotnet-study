let checkIfPangram (sentence: string) : bool =
    (sentence
     |> Seq.fold (fun acc c -> Set.add c acc) Set.empty
     |> Seq.toList
     |> List.length) = 26

// true
checkIfPangram "thequickbrownfoxjumpsoverthelazydog"

// false
checkIfPangram "leetcode"
