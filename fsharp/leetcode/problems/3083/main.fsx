let isSubstringPresent (s: string) : bool =
    let chunks1 = s |> Seq.windowed 2
    let chunks2 = s |> Seq.rev |> Seq.windowed 2

    chunks2 |> Seq.exists (fun c -> Seq.exists (fun c2 -> c = c2) chunks1)

// true
isSubstringPresent "leetcode"

// true
isSubstringPresent "abcba"

// false
isSubstringPresent "abcd"
