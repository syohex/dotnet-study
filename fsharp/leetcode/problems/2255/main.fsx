let countPrefixes (words: string list) (s: string) : int =
    words
    |> List.filter (fun word -> s.StartsWith(word))
    |> List.length

// 3
countPrefixes [ "a"; "b"; "c"; "ab"; "bc"; "abc" ] "abc"

// 2
countPrefixes [ "a"; "a" ] "aa"
