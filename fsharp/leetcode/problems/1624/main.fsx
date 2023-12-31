open System

let maxLengthBetweenCharacters (s: string) : int =
    let rec maxLengthBetweenCharacters' cs m acc =
        match cs with
        | [] -> acc
        | (i, h) :: t ->
            match Map.tryFind h m with
            | None -> maxLengthBetweenCharacters' t (Map.add h i m) acc
            | Some(v) -> maxLengthBetweenCharacters' t m (Math.Max(acc, i - v - 1))

    let cs = s |> Seq.indexed |> Seq.toList
    maxLengthBetweenCharacters' cs Map.empty -1

// 0
maxLengthBetweenCharacters "aa"

// 2
maxLengthBetweenCharacters "abca"

// -1
maxLengthBetweenCharacters "cbzxy"
