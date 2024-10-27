let countSquares (matrix: int[,]) : int =
    let rows, cols = Array2D.length1 matrix, Array2D.length2 matrix
    let dp = Array2D.zeroCreate (rows + 1) (cols + 1)

    [ for i in 0 .. (rows - 1) do
          for j in 0 .. (cols - 1) do
              if matrix.[i, j] = 1 then
                  dp.[i + 1, j + 1] <- 1 + min dp.[i, j] (min dp.[i + 1, j] dp.[i, j + 1])
                  yield dp.[i + 1, j + 1] ]
    |> List.sum

let matrix1 = array2D [ [ 0; 1; 1; 1 ]; [ 1; 1; 1; 1 ]; [ 0; 1; 1; 1 ] ]
// 15
countSquares matrix1

let matrix2 = array2D [ [ 1; 0; 1 ]; [ 1; 1; 0 ]; [ 1; 1; 0 ] ]
// 7
countSquares matrix2
