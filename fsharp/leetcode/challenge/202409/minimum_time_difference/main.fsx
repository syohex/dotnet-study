let findMinDifference (timePoints: string list) : int =
    let timePointToMinutes (t: string) : int =
        let t = t |> Seq.map (fun c -> int c - int '0') |> Seq.toArray

        match t with
        | [| a; b; _; c; d |] -> (10 * a + b) * 60 + 10 * c + d
        | _ -> failwithf "never reach here %A" t

    let rec findMinDifference' ts prev acc =
        match ts with
        | [] -> acc
        | h :: t -> findMinDifference' t h (min acc (h - prev))

    let ts = timePoints |> List.map timePointToMinutes |> List.sort

    match ts with
    | [] -> failwith "never reach here"
    | h :: t -> findMinDifference' (t @ [ h + 1440 ]) h 2000

// 1
findMinDifference [ "00:00"; "23:59" ]

// 0
findMinDifference [ "00:00"; "23:59"; "00:00" ]
