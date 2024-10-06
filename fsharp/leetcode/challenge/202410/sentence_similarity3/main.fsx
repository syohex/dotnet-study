let areSentencesSimilar (sentence1: string) (sentence2: string) : bool =
    let rec firstUnmatchIndex i w1 w2 =
        match w1, w2 with
        | [], []
        | [], _ -> i
        | _, [] -> failwith "never reach here"
        | h1 :: t1, h2 :: t2 -> if h1 = h2 then firstUnmatchIndex (i + 1) t1 t2 else i

    let w1, w2 =
        sentence1.Split(' ') |> List.ofArray, sentence2.Split(' ') |> List.ofArray

    let len1, len2 = w1.Length, w2.Length

    let w1, w2, len1 = if len1 < len2 then w1, w2, len1 else w2, w1, len2

    let si = firstUnmatchIndex 0 w1 w2
    let ei = len1 - 1 - (firstUnmatchIndex 0 (List.rev w1) (List.rev w2))
    ei < si

// true
areSentencesSimilar "Hello Jane" "Hello my name is Jane"

// false
areSentencesSimilar "Frog cool" "Frogs are cool"

// true
areSentencesSimilar "My name is Haley" "My Haley"

// false
areSentencesSimilar "of" "A lot of words"

// true
areSentencesSimilar "Eating right now" "Eating"
