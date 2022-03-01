let countBits (n: int) : int list =
    [ 0 .. n ]
    |> List.map (fun m -> sprintf "%B" m)
    |> List.map (fun str ->
        str
        |> Seq.fold (fun acc c -> acc + int c - int '0') 0)

// [0, 1, 1]
countBits 2

// [0, 1, 1, 2, 1, 2]
countBits 5
