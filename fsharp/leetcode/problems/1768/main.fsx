let mergeAlternately (word1: string) (word2: string) : string =
    let rec mergeAlternately' cs1 cs2 even (acc: char list) =
        match cs1, cs2 with
        | [], [] -> acc |> List.rev |> System.String.Concat
        | h :: t, [] -> mergeAlternately' t [] even (h :: acc)
        | [], h :: t -> mergeAlternately' [] t even (h :: acc)
        | h :: t, _ when even -> mergeAlternately' t cs2 false (h :: acc)
        | _, h :: t -> mergeAlternately' cs1 t true (h :: acc)

    mergeAlternately' (Seq.toList word1) (Seq.toList word2) true []

// "apbqcr"
mergeAlternately "abc" "pqr"

// "apbqrs"
mergeAlternately "ab" "pqrs"

// "apbqcd"
mergeAlternately "abcd" "pq"
