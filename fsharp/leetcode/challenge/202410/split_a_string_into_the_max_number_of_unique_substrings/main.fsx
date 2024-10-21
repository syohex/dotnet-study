let maxUniqueSplit (s: string) : int =
    let rec maxUniqueSplit' cs substrs =
        match cs with
        | [] -> Set.count substrs
        | cs ->
            cs
            |> List.indexed
            |> List.fold
                (fun (ret, acc) (i, c) ->
                    let acc = c :: acc

                    if Set.contains acc substrs then
                        ret, acc
                    else
                        let ret' = maxUniqueSplit' (List.skip (i + 1) cs) (Set.add acc substrs)
                        max ret ret', acc)
                (0, [])
            |> fst

    maxUniqueSplit' (Seq.toList s) Set.empty

// 5
maxUniqueSplit "ababccc"

// 2
maxUniqueSplit "aba"

// 1
maxUniqueSplit "aa"
