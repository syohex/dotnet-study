open System

let isVowel (c: char) : bool =
    let isVowel' c =
        match c with
        | 'a'
        | 'e'
        | 'i'
        | 'o'
        | 'u' -> true
        | _ -> false

    c |> Char.ToLower |> isVowel'

let halvesAreAlike (s: string) : bool =
    let half = s.Length / 2

    let a = s |> Seq.take half |> Seq.filter isVowel |> Seq.length
    let b = s |> Seq.skip half |> Seq.filter isVowel |> Seq.length

    a = b

// true
halvesAreAlike "book"

// false
halvesAreAlike "textbook"
