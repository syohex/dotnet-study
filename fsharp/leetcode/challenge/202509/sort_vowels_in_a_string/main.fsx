let sortVowels (s: string) : string =
    let isVowel c =
        match c with
        | 'a'
        | 'e'
        | 'i'
        | 'o'
        | 'u'
        | 'A'
        | 'E'
        | 'I'
        | 'O'
        | 'U' -> true
        | _ -> false

    let rec sortVowels' cs vowels (acc: char list) =
        match cs with
        | [] -> acc |> List.rev |> System.String.Concat
        | h :: t ->
            if isVowel h then
                sortVowels' t (List.tail vowels) (List.head vowels :: acc)
            else
                sortVowels' t vowels (h :: acc)

    let vowels = s |> Seq.filter isVowel |> Seq.sort |> Seq.toList
    sortVowels' (Seq.toList s) vowels []

// "lEOtcede"
sortVowels "lEetcOde"

// "lYmpH"
sortVowels "lYmpH"
