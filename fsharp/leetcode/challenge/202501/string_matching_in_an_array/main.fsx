let stringMatching (words: string list) : string list =
    let words = List.indexed words

    words
    |> List.fold
        (fun acc (i, word) ->
            match List.tryFind (fun (j, (w: string)) -> i <> j && w.Contains(word)) words with
            | Some(_) -> word :: acc
            | None -> acc)
        []

// ["as", "hero"]
stringMatching [ "mass"; "as"; "hero"; "superhero" ]

// ["et";"code"]
stringMatching [ "leetcode"; "et"; "code" ]

// []
stringMatching [ "blue"; "green"; "bu" ]

// ["leetcode", "od", "am"]
stringMatching [ "leetcoder"; "leetcode"; "od"; "hamlet"; "am" ]
