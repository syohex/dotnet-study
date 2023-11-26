let findWordsContaining (words: string list) (x: char) : int list =
    words
    |> List.indexed
    |> List.filter (fun (_, word) -> word.Contains(x))
    |> List.map fst

// [0, 1]
findWordsContaining [ "leet"; "code" ] 'e'

// [0, 2]
findWordsContaining [ "abc"; "bcd"; "aaaa"; "cbc" ] 'a'

// []
findWordsContaining [ "abc"; "bcd"; "aaaa"; "cbc" ] 'z'
