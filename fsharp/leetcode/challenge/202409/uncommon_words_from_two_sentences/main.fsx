let uncommonSentences (s1: string) (s2: string) : string list =
    let findUniqueWord m1 m2 acc =
        m1
        |> Map.fold
            (fun acc k v ->
                if v > 1 then
                    acc
                else
                    match Map.tryFind k m2 with
                    | Some(_) -> acc
                    | None -> k :: acc)
            acc

    let wordCount (s: string) =
        s.Split([| ' ' |]) |> Array.countBy id |> Map.ofArray

    let m1, m2 = wordCount s1, wordCount s2

    findUniqueWord m1 m2 [] |> findUniqueWord m2 m1

// Set["sweet","sour"]
uncommonSentences "this apple is sweet" "this apple is sour"

// Set["banana"]
uncommonSentences "apple apple" "banana"
