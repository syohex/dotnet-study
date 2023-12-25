let numDecodings (s: string) : int =
    let rec numDecodings' i cs cache =
        match cs with
        | [] -> 1, cache
        | h :: t ->
            match Map.tryFind i cache with
            | Some(v) -> v, cache
            | None ->
                let ret', cache' = numDecodings' (i + 1) t cache

                match t with
                | [] -> ret', Map.add i ret' cache'
                | h' :: t' ->
                    if h = '0' then
                        0, cache'
                    elif h = '1' || (h = '2' && h' <= '6') then
                        let ret'', cache'' = numDecodings' (i + 2) t' cache'
                        ret' + ret'', Map.add i (ret' + ret'') cache''
                    else
                        ret', Map.add i ret' cache'

    numDecodings' 0 (Seq.toList s) Map.empty |> fst

// 2
numDecodings "12"

// 3
numDecodings "226"

// 0
numDecodings "06"
