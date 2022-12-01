let halvesAreAlike (s: string) : bool =
    let isVowel c =
        let c' = System.Char.ToLower(c)

        c' = 'a'
        || c' = 'e'
        || c' = 'i'
        || c' = 'o'
        || c' = 'u'

    let countVowel cs =
        let rec countVowel' cs acc =
            match cs with
            | [] -> acc
            | h :: t -> countVowel' t (acc + if isVowel h then 1 else 0)

        countVowel' cs 0

    let half = s.Length / 2
    let cs = s |> Seq.toList
    let front = cs |> List.take half
    let last = cs |> List.skip half

    (countVowel front) = (countVowel last)

// true
halvesAreAlike "book"

// false
halvesAreAlike "textbook"
