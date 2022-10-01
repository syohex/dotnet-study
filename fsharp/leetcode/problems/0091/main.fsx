let numDecodings (s: string) : int =
    let rec numDecodings pos cs cache =
        match cs with
        | [] -> 1, cache
        | h :: [] ->
            match Map.tryFind pos cache with
            | Some (v) -> v, cache
            | None ->
                match h with
                | '0' -> 0, Map.add pos 0 cache
                | _ -> 1, Map.add pos 1 cache
        | h1 :: h2 :: t ->
            match Map.tryFind pos cache with
            | Some (v) -> v, cache
            | None ->
                match h1 with
                | '0' -> 0, Map.add pos 0 cache
                | '1' ->
                    let ret1, cache1 = numDecodings (pos + 1) (h2 :: t) cache
                    let ret2, cache2 = numDecodings (pos + 2) t cache1
                    ret1 + ret2, Map.add pos (ret1 + ret2) cache2
                | '2' ->
                    let ret1, cache1 = numDecodings (pos + 1) (h2 :: t) cache

                    let ret2, cache2 =
                        if h2 >= '0' && h2 <= '6' then
                            numDecodings (pos + 2) t cache1
                        else
                            0, cache1

                    ret1 + ret2, Map.add pos (ret1 + ret2) cache2
                | _ ->
                    let ret, cache' = numDecodings (pos + 1) t cache
                    ret, Map.add pos ret cache'

    numDecodings 0 (s |> Seq.toList) Map.empty |> fst

// 2
numDecodings "12"

// 3
numDecodings "226"

// 0
numDecodings "06"

// 3
numDecodings "1201234"
