let countSquares (matrix: int[,]) : int =
    let rows, cols = Array2D.length1 matrix, Array2D.length2 matrix

    let rec countSquares' (dp: int[,]) row col acc =
        if row >= rows then
            acc
        elif col >= cols then
            countSquares' dp (row + 1) 0 acc
        else if matrix.[row, col] = 1 then
            let v = 1 + min dp.[row, col] (min dp.[row + 1, col] dp.[row, col + 1])
            dp.[row + 1, col + 1] <- v
            countSquares' dp row (col + 1) (acc + v)
        else
            countSquares' dp row (col + 1) acc

    let dp = Array2D.zeroCreate (rows + 1) (cols + 1)
    countSquares' dp 0 0 0

let matrix1 = array2D [ [ 0; 1; 1; 1 ]; [ 1; 1; 1; 1 ]; [ 0; 1; 1; 1 ] ]
// 15
countSquares matrix1

let matrix2 = array2D [ [ 1; 0; 1 ]; [ 1; 1; 0 ]; [ 1; 1; 0 ] ]
// 7
countSquares matrix2
