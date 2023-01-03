let minDeletionSize (strs: string list) : int =
    seq { 0 .. (strs.Head.Length - 1) }
    |> Seq.map (fun i -> strs |> List.map (fun s -> s.[i]))
    |> Seq.filter (fun cs -> cs <> List.sort cs)
    |> Seq.length

// 1
minDeletionSize [ "cba"; "daf"; "ghi" ]

// 0
minDeletionSize [ "a"; "b" ]

// 3
minDeletionSize [ "zyx"; "wvu"; "tsr" ]

// 2
minDeletionSize [ "rrjk"
                  "furt"
                  "guzm" ]
