let removeOccurrences (s: string) (part: string) : string =
    let isPartMatched stack part partLen =
        let rec f stack part =
            match stack, part with
            | [], [] -> true, []
            | s, [] -> true, s
            | [], _ -> false, []
            | h1 :: t1, h2 :: t2 -> if h1 = h2 then f t1 t2 else false, []

        if List.length stack < partLen then
            false, stack
        else
            f stack part

    let rec removeOccurences' cs part partLen (stack: char list) =
        match cs with
        | [] -> stack |> List.rev |> System.String.Concat
        | h :: t ->
            let ok, stack' = isPartMatched (h :: stack) part partLen

            if ok then
                removeOccurences' t part partLen stack'
            else
                removeOccurences' t part partLen (h :: stack)

    let part, partLen = part |> Seq.toList |> List.rev, part.Length
    removeOccurences' (Seq.toList s) part partLen []

// "dab"
removeOccurrences "daabcbaabcbc" "abc"

// "ab"
removeOccurrences "axxxxyyyyb" "xy"
