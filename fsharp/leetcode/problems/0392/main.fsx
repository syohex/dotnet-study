let isSubsequence (s: string) (t: string) : bool =
    let rec isSubsequence' cs ct =
        match cs with
        | [] -> true
        | x :: xs ->
            match List.tryFindIndex ((=) x) ct with
            | Some (p) -> isSubsequence' xs (ct |> List.skip (p + 1))
            | None -> false

    isSubsequence' (s |> Seq.toList) (t |> Seq.toList)

// true
isSubsequence "abc" "ahbgdc"

// false
isSubsequence "axc" "ahbgdc"

// true
isSubsequence "ab" "baab"

// false
isSubsequence "aaaaaa" "bbaaaa"