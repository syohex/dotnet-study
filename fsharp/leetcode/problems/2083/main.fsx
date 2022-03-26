let toCharIndexes (s: string) : Map<char, int list> =
    let rec toCharIndexes' cs m =
        match cs with
        | [] -> m
        | (i, c) :: t ->
            match Map.tryFind c m with
            | None -> toCharIndexes' t (Map.add c [ i ] m)
            | Some (v) -> toCharIndexes' t (Map.add c (i :: v) m)

    let cs =
        s
        |> Seq.toList
        |> List.mapi (fun i c -> i, c)
        |> List.rev

    toCharIndexes' cs Map.empty

let numberOfSubstrings (s: string) : int64 =
    let charIndexes = toCharIndexes s

    charIndexes
    |> Map.fold
        (fun acc _ v ->
            let len = v.Length

            if len = 1 then
                acc + 1L
            else
                let n = (1 + len) * len / 2
                acc + int64 (n))
        0L


// 7
numberOfSubstrings "abcba"

// 9
numberOfSubstrings "abacad"

// 1
numberOfSubstrings "a"
