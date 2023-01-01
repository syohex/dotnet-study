let wordPattern (pattern: string) (s: string) : bool =
    let rec wordPattern' pattern words m registered =
        match pattern, words with
        | [], [] -> true
        | p1 :: t1, word :: t2 ->
            match Map.tryFind p1 m with
            | Some (v) ->
                if word <> v then
                    false
                else
                    wordPattern' t1 t2 m registered
            | None ->
                if Set.contains word registered then
                    false
                else
                    wordPattern' t1 t2 (Map.add p1 word m) (Set.add word registered)
        | _, _ -> failwith "never reach here"

    let words = s.Split ' '

    if pattern.Length <> words.Length then
        false
    else
        wordPattern' (pattern |> Seq.toList) (words |> Array.toList) Map.empty Set.empty

// true
wordPattern "abba" "dog cat cat dog"

// false
wordPattern "abba" "dog cat cat fish"

// false
wordPattern "aaaa" "dog cat cat dog"

// false
wordPattern "abba" "fish whoops helloworld fish"

// false
wordPattern "aaa" "aa aa aa aa"

// false
wordPattern "abba" "dog dog dog dog"
