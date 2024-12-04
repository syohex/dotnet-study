let canMakeSubsequence (str1: string) (str2: string) : bool =
    let toList = Seq.map (fun c -> int c - int 'a') >> Seq.toList
    let isOk a b = a = b || (a + 1) % 26 = b

    let rec canMakeSubsequence' s1 s2 =
        match s2 with
        | [] -> true
        | h :: t ->
            match List.tryFindIndex (fun n -> isOk n h) s1 with
            | None -> false
            | Some(i) -> canMakeSubsequence' (List.skip (i + 1) s1) t

    canMakeSubsequence' (toList str1) (toList str2)

// true
canMakeSubsequence "abc" "ad"

// true
canMakeSubsequence "zc" "ad"

// false
canMakeSubsequence "ab" "d"

// false
canMakeSubsequence "oh" "hu"
