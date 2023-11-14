let countPalindromicSubsequence (s: string) : int =
    let rec countPalindromicSubsequence' cs (s: char[]) acc =
        match cs with
        | [] -> acc
        | h :: t ->
            let first, last = Array.findIndex ((=) h) s, Array.findIndexBack ((=) h) s

            if first = last then
                countPalindromicSubsequence' t s acc
            else
                let uniqs = s.[(first + 1) .. (last - 1)] |> Set.ofArray |> Seq.length
                countPalindromicSubsequence' t s (acc + uniqs)

    let uniqChars = Set.ofSeq s |> Set.toList
    countPalindromicSubsequence' uniqChars (Seq.toArray s) 0

// 3
countPalindromicSubsequence "aabca"

// 0
countPalindromicSubsequence "adc"

// 4
countPalindromicSubsequence "bbcbaba"
