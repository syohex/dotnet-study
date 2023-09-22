let isSubsequence (s: string) (t: string) : bool =
    let rec isSubsequence' cs ts =
        match cs with
        | [] -> true
        | h :: rest ->
            match List.tryFindIndex ((=) h) ts with
            | None -> false
            | Some(idx) -> isSubsequence' rest (List.skip (idx + 1) ts)

    isSubsequence' (List.ofSeq s) (List.ofSeq t)

// true
isSubsequence "abc" "ahbgdc"

// false
isSubsequence "axc" "ahbgdc"

// false
isSubsequence "bbaaaaa" "aaaaaaa"

// true
isSubsequence "ab" "baab"
