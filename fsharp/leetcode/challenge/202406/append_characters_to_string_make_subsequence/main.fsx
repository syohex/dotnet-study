let appendCharacters (s: string) (t: string) : int =
    let rec appendCharacters' cs ts i =
        match cs, ts with
        | [], []
        | [], _
        | _, [] -> t.Length - i
        | h1 :: t1, h2 :: t2 ->
            if h1 = h2 then
                appendCharacters' t1 t2 (i + 1)
            else
                appendCharacters' t1 ts i

    appendCharacters' (Seq.toList s) (Seq.toList t) 0

// 4
appendCharacters "coaching" "coding"

// 0
appendCharacters "abcde" "a"

// 5
appendCharacters "z" "abcde"

// 7
appendCharacters "lxvqffcj" "vjtgatotj"

// 0
appendCharacters "lbg" "g"
