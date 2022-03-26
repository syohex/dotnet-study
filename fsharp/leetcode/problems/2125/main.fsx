let removeEmptyLines (bank: string list) : string list =
    bank
    |> List.filter (fun s -> s |> Seq.forall ((=) '0') |> not)

let toBeamCount (bank: string list) : int list =
    bank
    |> List.map (fun s ->
        s
        |> Seq.fold (fun acc c -> acc + (if c = '1' then 1 else 0)) 0)

let numberOfBeams (bank: string list) : int =
    let rec numberOfBeams' beams prev acc =
        match beams with
        | [] -> acc
        | h :: t -> numberOfBeams' t h (acc + (h * prev))

    let beams = bank |> removeEmptyLines |> toBeamCount

    match beams with
    | [] -> 0
    | h :: t -> numberOfBeams' t h 0

// 8
numberOfBeams [ "011001"
                "000000"
                "010100"
                "001000" ]

// 0
numberOfBeams [ "000"; "111"; "000" ]
