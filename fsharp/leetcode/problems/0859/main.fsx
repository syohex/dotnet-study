let buddyStrings (s: string) (goal: string) : bool =
    if s.Length <> goal.Length then
        false
    else
        let diffPairs =
            Seq.zip s goal
            |> Seq.fold (fun acc (a, b) -> if a <> b then (a, b) :: acc else acc) []
            |> Seq.toList

        let diffs = List.length diffPairs

        if diffs >= 3 || diffs = 1 then
            false
        elif diffs = 2 then
            match diffPairs with
            | (a1, b1) :: (a2, b2) :: [] -> a1 = b2 && b1 = a2
            | _ -> failwith "never reach here"
        else
            s
            |> Seq.fold
                (fun acc c ->
                    match Map.tryFind c acc with
                    | Some(v) -> Map.add c (v + 1) acc
                    | None -> Map.add c 1 acc)
                Map.empty
            |> Map.exists (fun _ v -> v >= 2)

// true
buddyStrings "ab" "ba"

// false
buddyStrings "ab" "ab"

// true
buddyStrings "aa" "aa"

// false
buddyStrings "aab" "aac"
