let maximumNumberOfStringPairs (words: string list) : int =
    let toFreq s =
        let rec toFreq' cs acc =
            match cs with
            | [] -> acc
            | h :: t ->
                match Map.tryFind h acc with
                | Some(v) -> toFreq' t (Map.add h (v + 1) acc)
                | None -> toFreq' t (Map.add h 1 acc)

        toFreq' (Seq.toList s) Map.empty

    let rec maximumNumberOfStringPairs' freqs acc =
        match freqs with
        | []
        | _ :: [] -> acc
        | h :: t ->
            let pairs = List.filter (fun f -> h = f) t |> List.length
            maximumNumberOfStringPairs' t (acc + pairs)

    let freqs = List.map toFreq words
    maximumNumberOfStringPairs' freqs 0

// 2
maximumNumberOfStringPairs [ "cd"; "ac"; "dc"; "ca"; "zz" ] |> printfn "%d"
// 1
maximumNumberOfStringPairs [ "ab"; "ba"; "cc" ] |> printfn "%d"
// 0
maximumNumberOfStringPairs [ "aa"; "ab" ] |> printfn "%d"
