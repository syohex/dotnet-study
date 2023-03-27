open System

let minPathSum (grid: int[,]) : int =
    let rows, cols = Array2D.length1 grid, Array2D.length2 grid
    let dp = Array2D.init rows cols (fun _ _ -> Int32.MaxValue)
    dp.[0, 0] <- grid.[0, 0]

    for i in 0 .. (rows - 1) do
        for j in 0 .. (cols - 1) do
            if i <> 0 then
                dp.[i, j] <- grid.[i, j] + dp.[i - 1, j]

            if j <> 0 then
                dp.[i, j] <- Math.Min(dp.[i, j], grid.[i, j] + dp.[i, j - 1])


    dp.[rows - 1, cols - 1]

let grid1 = array2D [ [ 1; 3; 1 ]; [ 1; 5; 1 ]; [ 4; 2; 1 ] ]
// 7
minPathSum grid1

let grid2 = array2D [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ]
// 12
minPathSum grid2
