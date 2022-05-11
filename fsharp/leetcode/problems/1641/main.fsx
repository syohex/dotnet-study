let countVowelStrings(n: int) : int =
    let mutable cache = Map.empty
    let rec countVowelStrings' n vowels =
        if n = 1 then
            vowels
        elif vowels = 1 then
            1
        else
            match Map.tryFind (n, vowels) cache  with
            | Some(v) -> v
            | None ->
                let ret1 = countVowelStrings' n (vowels - 1)
                let ret2 = countVowelStrings' (n - 1) vowels
                let ret = ret1 + ret2

                cache <- (Map.add (n, vowels) ret cache)
                ret

    countVowelStrings' n 5

// 5
countVowelStrings 1

// 15
countVowelStrings 2

// 66045
countVowelStrings 33
