let countSubmatrices (grid: int[,]) (k: int) : int =
    let rows, cols = Array2D.length1 grid, Array2D.length2 grid

    let rec f row col (acc: int[,]) ret =
        if row >= rows then
            ret
        elif col >= cols then
            f (row + 1) 0 acc ret
        else
            acc.[row + 1, col + 1] <- grid.[row, col] + acc.[row, col + 1] + acc.[row + 1, col] - acc.[row, col]

            if acc.[row + 1, col + 1] > k then
                f (row + 1) 0 acc ret
            else
                f row (col + 1) acc (ret + 1)

    let acc = Array2D.zeroCreate (rows + 1) (cols + 1)
    f 0 0 acc 0

let grid1 = array2D [ [ 7; 6; 3 ]; [ 6; 6; 1 ] ]
// 4
countSubmatrices grid1 18

let grid2 = array2D [ [ 7; 2; 9 ]; [ 1; 5; 0 ]; [ 2; 6; 6 ] ]
// 6
countSubmatrices grid2 20
