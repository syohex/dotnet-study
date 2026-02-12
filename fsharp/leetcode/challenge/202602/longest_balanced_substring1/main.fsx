let longestBalanced (s: string) : int =
    let isBalanced cs =
        cs
        |> Seq.fold
            (fun (acc, count) c ->
                let v = Map.tryFind c acc |> Option.defaultValue 0 |> ((+) 1)
                let acc = Map.add c v acc
                acc, max v count)
            (Map.empty, 0)
        |> fun (freq, count) -> freq |> Map.forall (fun _ v -> v = count)

    let rec f i acc =
        if i > s.Length then
            acc
        else
            let ok = s |> Seq.windowed i |> Seq.exists isBalanced
            if ok then f (i + 1) i else f (i + 1) acc

    f 1 0

// 4
longestBalanced "abbac"

// 4
longestBalanced "zzabccy"

// 2
longestBalanced "aba"
