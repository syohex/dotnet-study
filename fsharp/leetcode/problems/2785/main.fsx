open System

let sortVowels (s: string) : string =
    let isVowel (c: char) : bool =
        match c with
        | 'a'
        | 'A'
        | 'e'
        | 'E'
        | 'i'
        | 'I'
        | 'o'
        | 'O'
        | 'u'
        | 'U' -> true
        | _ -> false

    let rec sortVowels' (cs: char list) (vs: char list) (acc: char list) =
        match cs with
        | [] -> acc |> List.rev |> String.Concat
        | h :: t ->
            if isVowel h then
                sortVowels' t (List.tail vs) ((List.head vs) :: acc)
            else
                sortVowels' t vs (h :: acc)

    let vowels = s |> Seq.filter isVowel |> Seq.sort |> Seq.toList
    sortVowels' (Seq.toList s) vowels []

// "lEOtcede"
sortVowels "lEetcOde"

// "lYmph"
sortVowels "lYmph"
