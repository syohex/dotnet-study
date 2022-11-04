let reverseVowels (s: string) : string =
    let isVowel (c: char) : bool =
        let c = System.Char.ToLower(c)

        c = 'a'
        || c = 'e'
        || c = 'i'
        || c = 'o'
        || c = 'u'

    let rec nextVowel pos (cs: char []) checkFn nextStep =
        if checkFn pos then
            if isVowel cs.[pos] then
                pos
            else
                nextVowel (pos + nextStep) cs checkFn nextStep
        else
            pos

    let rec reverseVowels' left right (cs: char []) =
        if left >= right then
            cs |> System.String
        else
            let left' = nextVowel left cs (fun n -> n < cs.Length) 1
            let right' = nextVowel right cs (fun n -> n >= 0) -1

            if left' < right' then
                let tmp = cs.[left']
                cs.[left'] <- cs.[right']
                cs.[right'] <- tmp
                reverseVowels' (left' + 1) (right' - 1) cs
            else
                cs |> System.String

    reverseVowels' 0 (s.Length - 1) (s |> Seq.toArray)

// "holle"
reverseVowels "hello"

// "leotcede"
reverseVowels "leetcode"

// "Ui"
reverseVowels "iU"
