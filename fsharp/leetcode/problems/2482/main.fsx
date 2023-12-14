let onesMinusZeros (grid: int[,]) : int[,] =
    let rows, cols = Array2D.length1 grid, Array2D.length2 grid
    let rowOnes = Array.zeroCreate rows
    let colOnes = Array.zeroCreate cols

    for i in 0 .. (rows - 1) do
        for j in 0 .. (cols - 1) do
            rowOnes.[i] <- rowOnes.[i] + grid.[i, j]
            colOnes.[j] <- colOnes.[j] + grid.[i, j]

    let ret = Array2D.zeroCreate rows cols

    for i in 0 .. (rows - 1) do
        for j in 0 .. (cols - 1) do
            let x, y = rowOnes.[i], colOnes.[j]
            ret.[i, j] <- x + y - (rows - x) - (cols - y)

    ret

let grid1 = array2D [ [ 0; 1; 1 ]; [ 1; 0; 1 ]; [ 0; 0; 1 ] ]
// [[0,0,4],[0,0,4],[-2,-2,2]]
onesMinusZeros grid1

let grid2 = array2D [ [ 1; 1; 1 ]; [ 1; 1; 1 ] ]
// [[5,5,5],[5,5,5]]
onesMinusZeros grid2
