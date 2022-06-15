open System

let longestStrChain (words: string list) : int =
    let rec longestStrChain' (word: string) s cache : (int * Map<string, int>) =
        match Map.tryFind word cache with
        | Some (v) -> v, cache
        | None ->
            seq { 0 .. (word.Length - 1) }
            |> Seq.map (fun i ->
                if i = 0 then
                    word.Substring(1)
                elif i = word.Length - 1 then
                    word.Substring(0, word.Length - 1)
                else
                    word.Substring(0, i) + word.Substring(i + 1))
            |> Seq.filter (fun str -> Set.contains str s)
            |> Seq.fold
                (fun (acc, cache) substr ->
                    let ret, cache' = longestStrChain' substr s cache
                    Math.Max(acc, ret + 1), cache')
                (1, cache)

    let s =
        words
        |> List.fold (fun acc w -> Set.add w acc) Set.empty

    words
    |> List.fold
        (fun (acc, cache) w ->
            let ret, cache' = longestStrChain' w s cache
            Math.Max(ret, acc), cache')
        (0, Map.empty)
    |> fst

// 4
longestStrChain [ "a"
                  "b"
                  "ba"
                  "bca"
                  "bda"
                  "bdca" ]

// 5
longestStrChain [ "xbc"
                  "pcxbcf"
                  "xb"
                  "cxbc"
                  "pcxbc" ]

// 1
longestStrChain [ "abcd"; "dbqca" ]
