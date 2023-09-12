open System

let minDeletions (s: string) : int =
    let rec minDeletions' i countTable acc =
        if i <= 0 then
            acc
        else
            match Map.tryFind i countTable with
            | None -> minDeletions' (i - 1) countTable acc
            | Some(v) ->
                let countTable' =
                    if v = 1 then
                        countTable
                    else
                        match Map.tryFind (i - 1) countTable with
                        | Some(v') -> Map.add (i - 1) (v' + v - 1) countTable
                        | None -> Map.add (i - 1) (v - 1) countTable

                minDeletions' (i - 1) countTable' (acc + v - 1)

    let freq =
        s
        |> Seq.fold
            (fun acc c ->
                match Map.tryFind c acc with
                | Some(v) -> Map.add c (v + 1) acc
                | None -> Map.add c 1 acc)
            Map.empty

    let countTable =
        freq
        |> Map.fold
            (fun acc _ count ->
                match Map.tryFind count acc with
                | Some(chars) -> Map.add count (chars + 1) acc
                | None -> Map.add count 1 acc)
            Map.empty

    let maxCounts = countTable |> Map.fold (fun acc k _ -> Math.Max(acc, k)) 0
    minDeletions' maxCounts countTable 0

// 0
minDeletions "aab"

// 2
minDeletions "aaabbbcc"

// 2
minDeletions "ceabaacb"

// 1
minDeletions "accdcdadddbaadbc"

// 2
minDeletions "bbcebab"
