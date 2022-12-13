let minFallingPathSum (matrix: int[,]) : int =
    let rows = Array2D.length1 matrix
    let cols = Array2D.length2 matrix
    let dp = Array2D.create rows cols None

    seq { 0 .. (cols - 1) } |> Seq.iter (fun i -> dp.[0, i] <- Some(matrix.[0, i]))

    for i in 1 .. (rows - 1) do
        for j in 0 .. (cols - 1) do
            if j >= 1 then
                dp.[i, j] <- Some(dp.[i - 1, j - 1].Value + matrix.[i, j])

            if Option.isSome dp.[i, j] then
                dp.[i, j] <- Some(System.Math.Min(dp.[i, j].Value, dp.[i - 1, j].Value + matrix.[i, j]))
            else
                dp.[i, j] <- Some(dp.[i - 1, j].Value + matrix.[i, j])

            if j < cols - 1 then
                dp.[i, j] <- Some(System.Math.Min(dp.[i, j].Value, dp.[i - 1, j + 1].Value + matrix.[i, j]))

    dp.[rows - 1, *] |> Array.map (fun v -> v.Value) |> Array.min

let matrix1 = array2D [ [ 2; 1; 3 ]; [ 6; 5; 4 ]; [ 7; 8; 9 ] ]
// 13
minFallingPathSum matrix1

let matrix2 = array2D [ [ -19; 57 ]; [ -40; -5 ] ]
// -59
minFallingPathSum matrix2
