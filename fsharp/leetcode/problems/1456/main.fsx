let maxVowels (s: string) (k: int) : int =
    let isVowel (c: char) : bool =
        c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u'

    s
    |> Seq.windowed k
    |> Seq.map (fun w -> Seq.filter isVowel w |> Seq.length)
    |> Seq.max

// 3
maxVowels "abciiidef" 3

// 2
maxVowels "aeiou" 2

// 2
maxVowels "leetcode" 3
