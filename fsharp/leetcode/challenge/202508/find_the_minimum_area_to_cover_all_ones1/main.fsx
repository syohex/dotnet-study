let minimumArea (grid: int[,]) : int =
    let rows, cols = Array2D.length1 grid, Array2D.length2 grid

    let rec minimumArea' row col minRow minCol maxRow maxCol =
        if row >= rows then
            (maxRow - minRow + 1) * (maxCol - minCol + 1)
        elif col >= cols then
            minimumArea' (row + 1) 0 minRow minCol maxRow maxCol
        elif grid.[row, col] = 1 then
            minimumArea' row (col + 1) (min minRow row) (min minCol col) (max maxRow row) (max maxCol col)
        else
            minimumArea' row (col + 1) minRow minCol maxRow maxCol

    minimumArea' 0 0 rows cols 0 0

// 6
minimumArea (array2D [ [ 0; 1; 0 ]; [ 1; 0; 1 ] ])

// 1
minimumArea (array2D [ [ 1; 0 ]; [ 0; 0 ] ])
