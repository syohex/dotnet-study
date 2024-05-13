let matrixScore (grid: int[,]) : int =
    let rows, cols = Array2D.length1 grid, Array2D.length2 grid
    // flip row
    for i in 0 .. (rows - 1) do
        if grid.[i, 0] = 0 then
            for j in 0 .. (cols - 1) do
                grid.[i, j] <- if grid.[i, j] = 0 then 1 else 0

    // flip col
    for j in 1 .. (cols - 1) do
        let sum = seq { 0 .. (rows - 1) } |> Seq.fold (fun acc i -> acc + grid.[i, j]) 0

        if sum < rows - sum then
            for i in 0 .. (rows - 1) do
                grid.[i, j] <- if grid.[i, j] = 0 then 1 else 0

    seq { 0 .. (rows - 1) }
    |> Seq.fold
        (fun acc i ->
            acc
            + (seq { 0 .. (cols - 1) } |> Seq.fold (fun sum j -> sum * 2 + grid.[i, j]) 0))
        0

let grid1 = array2D [ [ 0; 0; 1; 1 ]; [ 1; 0; 1; 0 ]; [ 1; 1; 0; 0 ] ]
// 39
matrixScore grid1

// 1
matrixScore (array2D [ [ 0 ] ])
