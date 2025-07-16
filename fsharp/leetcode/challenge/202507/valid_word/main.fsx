let isValid (word: string) :bool =
    let rec isValid' cs hasVowel hasConsonant =
        match cs with
        | [] -> hasVowel && hasConsonant
        | h :: t ->
            match h with
            | 'a' | 'e' | 'i' | 'o' | 'u' -> isValid' t true hasConsonant
            | '@' | '#' | '$' -> false
            | h ->
                if h >= '0' && h <= '9' then
                    isValid' t hasVowel hasConsonant
                else
                    isValid' t hasVowel true

    if word.Length < 3 then
        false
    else
        let cs = word.ToLower() |> Seq.toList
        isValid' cs false false

// true
isValid "234Adas"

// false
isValid "b3"

// false
isValid "a3$e"

// false
isValid "UuE6"

// true
isValid "AhI"
