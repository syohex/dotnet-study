let strangePrinter (s: string) : int =
    let rec removeDup cs prev acc =
        match cs with
        | [] -> List.rev acc
        | h :: t ->
            if h = prev then
                removeDup t prev acc
            else
                removeDup t h (h :: acc)

    let rec strangePrinter' left right (cs: char[]) cache =
        if left > right then
            0, cache
        else
            match Map.tryFind (left, right) cache with
            | Some(v) -> v, cache
            | None ->
                let ret, cache = strangePrinter' (left + 1) right cs cache
                let ret = ret + 1

                let ret, cache =
                    seq { left + 1 .. right }
                    |> Seq.fold
                        (fun (acc, cache) i ->
                            if cs.[left] = cs.[i] then
                                let ret1, cache = strangePrinter' left (i - 1) cs cache
                                let ret2, cache = strangePrinter' (i + 1) right cs cache
                                min acc (ret1 + ret2), cache
                            else
                                acc, cache)
                        (ret, cache)

                ret, Map.add (left, right) ret cache

    match Seq.toList s with
    | [] -> failwith "never reach here"
    | h :: t ->
        let cs = removeDup t h [ h ] |> List.toArray
        strangePrinter' 0 (cs.Length - 1) cs Map.empty |> fst

// 2
strangePrinter "aaabbb"

// 2
strangePrinter "aba"
