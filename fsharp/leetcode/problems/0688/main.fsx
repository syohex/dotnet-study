let knightProbability (n: int) (k: int) (row: int) (column: int) : double =
    let isValidPosition (row, col) =
        row >= 0 && row < n && col >= 0 && col < n

    let dp = Array.init (k + 1) (fun _ -> Array2D.init n n (fun _ _ -> 0.0))

    let steps =
        [ (-1, -2); (-2, -1); (-2, 1); (-1, 2); (1, 2); (2, 1); (2, -1); (1, -2) ]

    dp.[0].[row, column] <- 1.0

    for step in 1..k do
        for i in 0 .. (n - 1) do
            for j in 0 .. (n - 1) do
                steps
                |> List.map (fun (x, y) -> i - x, j - y)
                |> List.filter isValidPosition
                |> List.iter (fun (x, y) -> dp.[step].[i, j] <- dp.[step].[i, j] + dp.[step - 1].[x, y] / 8.0)

    dp.[k] |> Seq.cast<double> |> Seq.sum

// 0.06250
knightProbability 3 2 0 0

// 1.0
knightProbability 1 0 0 0
