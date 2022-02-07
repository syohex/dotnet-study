let strToMap (s: string) : Map<char, int> =
    s
    |> Seq.toList
    |> List.fold
        (fun m c ->
            match Map.tryFind c m with
            | Some count -> Map.add c (count + 1) m
            | None -> Map.add c 1 m)
        Map.empty


let findTheDifference (s: string) (t: string) : char =
    let rec findTheDifference' cs sm tm =
        match cs with
        | [] -> failwith "never reach here"
        | head :: tail ->
            match (Map.tryFind head sm), (Map.tryFind head tm) with
            | Some a, Some b when a <> b -> head
            | Some _, Some _ -> findTheDifference' tail sm tm
            | None, Some _ -> head
            | _, _ -> failwith "never reach here"

    let sm = strToMap s
    let tm = strToMap t

    findTheDifference' (tm |> Map.keys |> Seq.toList) sm tm

// 'e'
findTheDifference "abcd" "abcde"

// 'y'
findTheDifference "" "y"
