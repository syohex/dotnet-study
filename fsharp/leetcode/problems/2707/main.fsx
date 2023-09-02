open System

let minExtraCharacters (s: string) (dictionary: string list) =
    let rec minExtraCharacters' i (s: string) (dict: Set<string>) (cache: Map<int, int>) =
        if i >= s.Length then
            0, cache
        else
            match Map.tryFind i cache with
            | Some(v) -> v, cache
            | None ->
                let ret, cache' = minExtraCharacters' (i + 1) s dict cache
                let ret = ret + 1 // unused case

                let ret', cache'' =
                    seq { i .. (s.Length - 1) }
                    |> Seq.fold
                        (fun (acc, cache) j ->
                            let substr = s.Substring(i, j - i + 1)

                            if Set.contains substr dict then
                                let ret, cache' = minExtraCharacters' (j + 1) s dict cache
                                Math.Min(acc, ret), cache'
                            else
                                acc, cache)
                        (ret, cache')

                ret', Map.add i ret' cache''

    let dict = Set.ofList dictionary
    minExtraCharacters' 0 s dict Map.empty |> fst

// 1
minExtraCharacters "leetscode" [ "leet"; "code"; "leetcode" ]

// 3
minExtraCharacters "sayhelloworld" [ "hello"; "world" ]
