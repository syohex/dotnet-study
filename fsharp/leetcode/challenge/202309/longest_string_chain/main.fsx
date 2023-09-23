open System

let subwords word =
    let rec subwords' i (word: string) acc =
        if i >= word.Length then
            acc
        else
            let subword =
                match i with
                | 0 -> word.[1..]
                | n when n = word.Length - 1 -> word.[.. n - 1]
                | n -> word.[.. n - 1] + word.[n + 1 ..]

            subwords' (i + 1) word (subword :: acc)

    subwords' 0 word []

let longestStringChain (words: string list) : int =
    let rec longestStringChain' word words cache =
        match Map.tryFind word cache with
        | Some(v) -> v, cache
        | None ->
            let ret, cache' =
                subwords word
                |> List.fold
                    (fun (acc, cache) subword ->
                        if Set.contains subword words then
                            let ret, cache' = longestStringChain' subword words cache
                            Math.Max(acc, ret + 1), cache'
                        else
                            acc, cache)
                    (1, cache)

            ret, Map.add word ret cache'

    let words' = Set.ofList words

    words'
    |> Set.fold
        (fun (acc, cache) word ->
            let ret, cache' = longestStringChain' word words' cache
            Math.Max(acc, ret), cache')
        (0, Map.empty)
    |> fst

// 4
longestStringChain [ "a"; "b"; "ba"; "bca"; "bda"; "bdca" ]

// 5
longestStringChain [ "xbc"; "pcxbcd"; "xb"; "cxbc"; "pcxbc" ]

// 1
longestStringChain [ "abcd"; "dbqca" ]
