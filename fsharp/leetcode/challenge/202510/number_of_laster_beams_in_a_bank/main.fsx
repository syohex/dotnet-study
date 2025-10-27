let numberOfBeams (bank: string list) : int =
    bank
    |> List.map (fun s -> s |> Seq.filter ((=) '1') |> Seq.length)
    |> List.filter ((<>) 0)
    |> List.fold (fun (acc, prev) n -> acc + prev * n, n) (0, 0)
    |> fst

// 8
numberOfBeams [ "011001"; "000000"; "010100"; "001000" ]

// 0
numberOfBeams [ "000"; "111"; "000" ]

// 0
numberOfBeams [ "0" ]
