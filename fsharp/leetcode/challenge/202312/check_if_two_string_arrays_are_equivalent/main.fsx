let arrayStringsAreEqual (word1: string list) (word2: string list) : bool =
    let rec f i1 (word1: string list) i2 (word2: string list) =
        match word1, word2 with
        | [], [] -> true
        | _, []
        | [], _ -> false
        | w1 :: t1, w2 :: t2 ->
            if w1.[i1] = w2.[i2] then
                if i1 + 1 >= w1.Length && i2 + 1 >= w2.Length then
                    f 0 t1 0 t2
                elif i1 + 1 >= w1.Length then
                    f 0 t1 (i2 + 1) word2
                elif i2 + 1 >= w2.Length then
                    f (i1 + 1) word1 0 t2
                else
                    f (i1 + 1) word1 (i2 + 1) word2
            else
                false

    f 0 word1 0 word2

// true
arrayStringsAreEqual [ "ab"; "c" ] [ "a"; "bc" ]

// false
arrayStringsAreEqual [ "a"; "cb" ] [ "ab"; "c" ]

// true
arrayStringsAreEqual [ "abc"; "d"; "defg" ] [ "abcddefg" ]
