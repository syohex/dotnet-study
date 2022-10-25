let arrayStringsAreEqual (word1: string list) (word2: string list) : bool =
    let w1 = word1 |> System.String.Concat
    let w2 = word2 |> System.String.Concat
    w1 = w2

// true
arrayStringsAreEqual [ "ab"; "c" ] [
    "a"
    "bc"
]

// false
arrayStringsAreEqual [ "a"; "cb" ] [
    "ab"
    "c"
]

// true
arrayStringsAreEqual [ "abc"; "d"; "defg" ] [
    "abcddefg"
]
