let repeatedSubstringPattern (s: string) : bool =
    let len = s.Length

    seq { 1 .. (len / 2) }
    |> Seq.filter (fun n -> len % n = 0)
    |> Seq.exists (fun n -> (s.Substring(0, n) |> String.replicate (len / n)) = s)

// true
repeatedSubstringPattern "abab"

// false
repeatedSubstringPattern "aba"

// false
repeatedSubstringPattern "a"

// true
repeatedSubstringPattern "abcabcabc"
