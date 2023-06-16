let isFascinating (n: int) : bool =
    let rec isFascinating' cs m =
        match cs with
        | [] -> true
        | h :: t ->
            match Map.tryFind h m with
            | Some(_) -> false
            | None -> isFascinating' t (Map.add h true m)

    let cs = (string n + string (n * 2) + string (n * 3)) |> Seq.toList
    isFascinating' cs (Map.empty |> Map.add '0' true)

// true
isFascinating 192

// false
isFascinating 100

// false
isFascinating 267
