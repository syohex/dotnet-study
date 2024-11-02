let isCircularSentence (sentence: string) : bool =
    let words = sentence.Split(' ') |> Array.toList

    words
    |> List.fold
        (fun (acc, (prev: string)) word ->
            if acc then
                word.[0] = prev.[prev.Length - 1], word
            else
                false, word)
        (true, List.last words)
    |> fst

// true
isCircularSentence "leetcode exercises sound delightful"

// true
isCircularSentence "eetcode"

// false
isCircularSentence "Leetcode is cool"

// false
isCircularSentence "leetcode"
