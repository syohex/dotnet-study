let areaOfMaxDiagonal (dimensions: (int * int) list) : int =
    dimensions
    |> List.fold
        (fun (ret, maxDiag) (width, height) ->
            let diag = sqrt <| double (width * width + height * height)

            if diag > maxDiag then width * height, diag
            elif diag = maxDiag then max ret (width * height), maxDiag
            else ret, maxDiag)
        (0, 0)
    |> fst

// 48
areaOfMaxDiagonal [ (9, 3); (8, 6) ]

// 12
areaOfMaxDiagonal [ (4, 3); (3, 4) ]
