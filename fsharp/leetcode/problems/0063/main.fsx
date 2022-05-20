let uniquePathsWithObstacles (obstableGrid: int [,]) : int =
    let rows = Array2D.length1 obstableGrid
    let cols = Array2D.length2 obstableGrid

    let dp = Array2D.zeroCreate rows cols

    if obstableGrid[0, 0] = 1 then
        0
    else
        dp.[0, 0] <- 1

        for i in 0 .. rows - 1 do
            for j in 0 .. cols - 1 do
                if obstableGrid[i, j] = 0 then
                    if i >= 1 then
                        dp.[i, j] <- dp.[i - 1, j]

                    if j >= 1 then
                        dp.[i, j] <- dp.[i, j] + dp.[i, j - 1]

        dp.[rows - 1, cols - 1]

let obstableGrid1 =
    array2D [ [ 0; 0; 0 ]
              [ 0; 1; 0 ]
              [ 0; 0; 0 ] ]
// 2
uniquePathsWithObstacles obstableGrid1

let obstableGrid2 = array2D [ [ 0; 1 ]; [ 0; 0 ] ]
// 1
uniquePathsWithObstacles obstableGrid2

let obstableGrid3 = array2D [ [ 1 ] ]
// 0
uniquePathsWithObstacles obstableGrid3
