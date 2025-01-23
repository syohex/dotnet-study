let countServers (grid: int[,]) : int =
    let rows, cols = Array2D.length1 grid, Array2D.length2 grid
    let rowSum, colSum = Array.zeroCreate rows, Array.zeroCreate cols

    for i in 0 .. rows - 1 do
        for j in 0 .. cols - 1 do
            if grid.[i, j] = 1 then
                rowSum.[i] <- rowSum.[i] + 1
                colSum.[j] <- colSum.[j] + 1

    seq {
        for i in 0 .. rows - 1 do
            for j in 0 .. cols - 1 do
                if grid.[i, j] = 1 then
                    yield (i, j)
    }
    |> Seq.fold (fun acc (i, j) -> acc + if rowSum.[i] >= 2 || colSum.[j] >= 2 then 1 else 0) 0

let grid1 = array2D [ [ 1; 0 ]; [ 0; 1 ] ]
// 0
countServers grid1

let grid2 = array2D [ [ 1; 0 ]; [ 1; 1 ] ]
// 3
countServers grid2

let grid3 =
    array2D [ [ 1; 1; 0; 0 ]; [ 0; 0; 1; 0 ]; [ 0; 0; 1; 0 ]; [ 0; 0; 0; 1 ] ]
// 4
countServers grid3
