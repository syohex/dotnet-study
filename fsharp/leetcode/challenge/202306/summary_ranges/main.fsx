let summaryRanges (nums: int list) : string list =
    let rec summaryRanges' nums start prev acc =
        match nums with
        | [] ->
            if start = prev then
                (string start) :: acc |> List.rev
            else
                (sprintf "%d->%d" start prev) :: acc |> List.rev
        | h :: t ->
            if (h - 1) = prev then
                summaryRanges' t start h acc
            else
                let acc' =
                    if start = prev then
                        (string start) :: acc
                    else
                        (sprintf "%d->%d" start prev) :: acc
                summaryRanges' t h h acc'

    match nums with
    | [] -> []
    | h :: t -> summaryRanges' t h h []

// ["0->2","4->5","7"]
summaryRanges [ 0; 1; 2; 4; 5; 7 ] |> printfn "%A"

// ["0","2->4","6","8->9"]
summaryRanges [ 0; 2; 3; 4; 6; 8; 9 ] |> printfn "%A"

// []
summaryRanges  [] |> printfn "%A"

// ["0","2","4","6"]
summaryRanges [ 0; 2; 4; 6 ] |> printfn "%A"
