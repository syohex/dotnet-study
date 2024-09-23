let minExtraChar (s: string) (dictionary: string list) : int =
    let rec minExtraChar' i words cache =
        if i >= s.Length then
            0, cache
        else
            match Map.tryFind i cache with
            | Some(v) -> v, cache
            | None ->
                let ret, cache = minExtraChar' (i + 1) words cache

                seq { i .. (s.Length - 1) }
                |> Seq.fold
                    (fun (acc, cache) j ->
                        let tmp = s.Substring(i, j - i + 1)

                        if Set.contains tmp words then
                            let ret, cache = minExtraChar' (j + 1) words cache
                            min acc ret, cache
                        else
                            acc, cache)
                    ((ret + 1), cache)

    let words = Set.ofList dictionary
    minExtraChar' 0 words Map.empty |> fst

// 1
minExtraChar "leetscode" [ "leet"; "code"; "leetcode" ]

// 3
minExtraChar "sayhelloworld" [ "hello"; "world" ]
