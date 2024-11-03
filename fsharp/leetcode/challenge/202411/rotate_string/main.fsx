let rotateString (s: string) (goal: string) : bool =
    if s.Length <> goal.Length then
        false
    else
        let len = s.Length
        let limit = len - 1

        seq { 0..limit }
        |> Seq.exists (fun i -> seq { 0..limit } |> Seq.forall (fun j -> s.[j] = goal.[(j + i) % len]))

// true
rotateString "abcde" "cdeab"

// false
rotateString "abcde" "abced"

// true
rotateString "defdefdefabcabc" "defdefabcabcdef"

// true
rotateString "bbbacddceeb" "ceebbbbacdd"

// false
rotateString "ab" "a"
