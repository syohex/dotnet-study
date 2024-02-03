let countKeyChanges (s: string) : int =
    let cs = s.ToLower() |> Seq.toList

    match cs with
    | [] -> failwith "never reach here"
    | h :: t ->
        t
        |> List.fold (fun (acc, prev) c -> if prev <> c then acc + 1, c else acc, c) (0, h)
        |> fst

// 2
countKeyChanges "aAbBcC"

// 0
countKeyChanges "AaaAaAaaAaaaAAAA"
