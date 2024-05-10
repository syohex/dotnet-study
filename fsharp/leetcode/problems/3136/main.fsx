let isValid (word: string) : bool =
    let rec isValid' cs hasVowel hasConsonant =
        match cs with
        | [] -> hasVowel && hasConsonant
        | h :: t ->
            let c = System.Char.ToLower(h)

            if c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u' then
                isValid' t true hasConsonant
            elif c >= '0' && c <= '9' then
                isValid' t hasVowel hasConsonant
            elif c = '@' || c = '#' || c = '$' then
                false
            else
                isValid' t hasVowel true

    if word.Length < 3 then
        false
    else
        isValid' (Seq.toList word) false false

// true
isValid "234Adas"

// false
isValid "b3"

// false
isValid "a3$e"
