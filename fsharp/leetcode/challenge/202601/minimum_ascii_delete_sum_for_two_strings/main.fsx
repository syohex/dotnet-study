let minimumDeleteSum (s1: string) (s2: string) : int =
    let rec f i j cs1 cs2 cache =
        match Map.tryFind (i, j) cache with
        | Some v -> v, cache
        | None ->
            match cs1, cs2 with
            | [], [] -> 0, cache
            | [], h :: t ->
                let v, cache = f i (j + 1) cs1 t cache
                let ret = int h + v
                ret, Map.add (i, j) ret cache
            | h :: t, [] ->
                let v, cache = f (i + 1) j t cs2 cache
                let ret = int h + v
                ret, Map.add (i, j) ret cache
            | h1 :: t1, h2 :: t2 ->
                if h1 = h2 then
                    let ret, cache = f (i + 1) (j + 1) t1 t2 cache
                    ret, Map.add (i, j) ret cache
                else
                    let ret1, cache = f (i + 1) j t1 cs2 cache
                    let ret2, cache = f i (j + 1) cs1 t2 cache
                    let ret = min (int h1 + ret1) (int h2 + ret2)
                    ret, Map.add (i, j) ret cache

    let cs1 = Seq.toList s1
    let cs2 = Seq.toList s2
    f 0 0 cs1 cs2 Map.empty |> fst

// 231
minimumDeleteSum "sea" "eat"

// 403
minimumDeleteSum "delete" "leet"
