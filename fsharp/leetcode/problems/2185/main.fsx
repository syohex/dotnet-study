let prefixCount (words: string list) (pref: string) : int =
    words
    |> List.filter (fun word -> word.StartsWith(pref))
    |> List.length

// 2
prefixCount
    [ "pay"
      "attention"
      "practice"
      "attend" ]
    "at"

// 0
prefixCount
    [ "leetcode"
      "win"
      "loops"
      "success" ]
    "code"
