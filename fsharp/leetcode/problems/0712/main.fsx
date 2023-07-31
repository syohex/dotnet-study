open System

let minimumDeleteSum (s1: string) (s2: string) : int =
    let rec minimumDeleteSum' (cs1: char list) (cs2: char list) i j cache =
        match Map.tryFind (i, j) cache with
        | Some(v) -> v, cache
        | None ->
            match cs1, cs2 with
            | [], [] -> 0, cache
            | h :: t, [] ->
                let ret, cache' = minimumDeleteSum' t [] (i - 1) j cache
                ret + int h, Map.add (i, j) (ret + int h) cache'
            | [], h :: t ->
                let ret, cache' = minimumDeleteSum' t [] i (j - 1) cache
                (ret + int h), Map.add (i, j) (ret + int h) cache'
            | h1 :: t1, h2 :: t2 ->
                if h1 = h2 then
                    let ret, cache' = minimumDeleteSum' t1 t2 (i - 1) (j - 1) cache
                    ret, Map.add (i, j) ret cache'
                else
                    let ret1, cache' = minimumDeleteSum' t1 cs2 (i - 1) j cache
                    let ret2, cache'' = minimumDeleteSum' cs1 t2 i (j - 1) cache'

                    let ret = Math.Min(ret1 + int h1, ret2 + int h2)
                    ret, Map.add (i, j) ret cache''

    let cs1, cs2 = s1 |> Seq.toList |> List.rev, s2 |> Seq.toList |> List.rev
    minimumDeleteSum' cs1 cs2 (s1.Length - 1) (s2.Length - 1) Map.empty |> fst

// 231
minimumDeleteSum "sea" "eat"

// 403
minimumDeleteSum "delete" "leet"
