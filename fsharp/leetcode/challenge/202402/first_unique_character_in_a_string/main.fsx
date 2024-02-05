let firstUniqueChar (s: string) : int =
    let m =
        s
        |> Seq.indexed
        |> Seq.fold
            (fun acc (i, c) ->
                match Map.tryFind c acc with
                | None -> Map.add c i acc
                | Some(v) -> Map.add c -1 acc)
            Map.empty
        |> Map.values
        |> Seq.filter (fun n -> n >= 0)

    if Seq.isEmpty m then -1 else Seq.min m

// 0
firstUniqueChar "leetcode"

// 2
firstUniqueChar "loveleetcode"

// -1
firstUniqueChar "aabb"
