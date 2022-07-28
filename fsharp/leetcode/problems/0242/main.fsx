let isAnagram (s: string) (t: string) : bool =
    let rec isAnagram' ss tt =
        match ss, tt with
        | [], [] -> true
        | [], _
        | _, [] -> false
        | h1 :: t1, h2 :: t2 ->
            if h1 = h2 then
                isAnagram' t1 t2
            else
                false

    isAnagram' (s |> Seq.sort |> Seq.toList) (t |> Seq.sort |> Seq.toList)

// true
isAnagram "anagram" "nagaram"

// false
isAnagram "rat" "car"
