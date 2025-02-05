let areAlmostEqual (s1: string) (s2: string) : bool =
    let v = Seq.zip s1 s2 |> Seq.filter (fun (c1, c2) -> c1 <> c2) |> Seq.toList

    match v with
    | [] -> true
    | _ :: [] -> false
    | (a1, a2) :: (b1, b2) :: [] when a1 = b2 && a2 = b1 -> true
    | _ -> false

// true
areAlmostEqual "bank" "kanb"

// false
areAlmostEqual "attack" "defend"

// true
areAlmostEqual "kelb" "kelb"

// false
areAlmostEqual "aa" "ac"

// false
areAlmostEqual "abab" "baba"
