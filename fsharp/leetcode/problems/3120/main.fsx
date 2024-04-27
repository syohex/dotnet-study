open System

let numberOfSpecialChars (word: string) : int =
    let rec numberOfSpecialChars' cs uppers lowers visited ret =
        match cs with
        | [] -> ret
        | h :: t ->
            let index, lowers, uppers =
                if Char.IsAsciiLetterLower(h) then
                    let index = int h - int 'a'
                    index, Set.add index lowers, uppers
                else
                    let index = int h - int 'A'
                    index, lowers, Set.add index uppers

            if
                (not <| Set.contains index visited)
                && Set.contains index lowers
                && Set.contains index uppers
            then
                numberOfSpecialChars' t uppers lowers (Set.add index visited) (ret + 1)
            else
                numberOfSpecialChars' t uppers lowers visited ret

    numberOfSpecialChars' (Seq.toList word) Set.empty Set.empty Set.empty 0

// 3
numberOfSpecialChars "aaAbcBC"

// 0
numberOfSpecialChars "abc"

// 1
numberOfSpecialChars "abBCab"
