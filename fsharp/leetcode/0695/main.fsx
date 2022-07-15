let maxAreaOfIsland (grid: int [,]) : int =
    let rows = Array2D.length1 grid
    let cols = Array2D.length2 grid

    let rec f (grid: int [,]) row col =
        if row < 0 || row >= rows || col < 0 || col >= cols then
            0
        elif grid.[row, col] = 0 then
            0
        else
            grid.[row, col] <- 0

            1
            + (f grid (row - 1) col)
            + (f grid row (col - 1))
            + (f grid (row + 1) col)
            + (f grid row (col + 1))

    let mutable ret = 0

    for i in 0 .. (rows - 1) do
        for j in 0 .. (cols - 1) do
            ret <- System.Math.Max(ret, f grid i j)

    ret

let grid1 =
    array2D [ [ 0
                0
                1
                0
                0
                0
                0
                1
                0
                0
                0
                0
                0 ]
              [ 0
                0
                0
                0
                0
                0
                0
                1
                1
                1
                0
                0
                0 ]
              [ 0
                1
                1
                0
                1
                0
                0
                0
                0
                0
                0
                0
                0 ]
              [ 0
                1
                0
                0
                1
                1
                0
                0
                1
                0
                1
                0
                0 ]
              [ 0
                1
                0
                0
                1
                1
                0
                0
                1
                1
                1
                0
                0 ]
              [ 0
                0
                0
                0
                0
                0
                0
                0
                0
                0
                1
                0
                0 ]
              [ 0
                0
                0
                0
                0
                0
                0
                1
                1
                1
                0
                0
                0 ]
              [ 0
                0
                0
                0
                0
                0
                0
                1
                1
                0
                0
                0
                0 ] ]

// 6
maxAreaOfIsland grid1

// 0
maxAreaOfIsland (array2D [ [ 0; 0; 0; 0; 0; 0; 0; 0 ] ])

// 3
maxAreaOfIsland (array2D [ [ 0; 0; 0; 1; 1; 1; 0; 0 ] ])
