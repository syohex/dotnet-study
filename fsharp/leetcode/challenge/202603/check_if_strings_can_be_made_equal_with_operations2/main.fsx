let checkStrings (s1: string) (s2: string) : bool =
    let updateMap key1 key2 m =
        let v1 = Map.tryFind key1 m |> Option.defaultValue 0
        let m = Map.add key1 (v1 + 1) m
        let v2 = Map.tryFind key2 m |> Option.defaultValue 0
        Map.add key2 (v2 - 1) m

    let rec checkStrings' cs1 cs2 evens odds =
        match cs1, cs2 with
        | [], [] ->
            let allZero = Map.values >> Seq.forall ((=) 0)
            allZero evens && allZero odds
        | [], _
        | _, [] -> failwith "never reach here"
        | (i, c1) :: t1, c2 :: t2 ->
            if i % 2 = 0 then
                checkStrings' t1 t2 (updateMap c1 c2 evens) odds
            else
                checkStrings' t1 t2 evens (updateMap c1 c2 odds)

    let cs1 = s1 |> Seq.toList |> List.indexed
    let cs2 = s2 |> Seq.toList
    checkStrings' cs1 cs2 Map.empty Map.empty

// true
checkStrings "abcdba" "cabdab"

// false
checkStrings "abe" "bea"
