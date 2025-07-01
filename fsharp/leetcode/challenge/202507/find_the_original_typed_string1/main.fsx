let possibleStringCount (word: string) : int =
    let rec possibleStringCount' cs prev count acc =
        match cs with
        | [] ->
            let acc = count :: acc
            acc |> List.fold (fun acc n -> acc + n - 1) 1
        | h :: t ->
            if prev = h then
                possibleStringCount' t prev (count + 1) acc
            else
                possibleStringCount' t h 1 (count :: acc)

    let cs = Seq.toList word
    possibleStringCount' (List.tail cs) (List.head cs) 1 []

// 5
possibleStringCount "abbcccc"

// 1
possibleStringCount "abcd"

// 4
possibleStringCount "aaaa"
