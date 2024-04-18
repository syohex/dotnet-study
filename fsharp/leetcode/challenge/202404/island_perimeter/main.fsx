let islandPerimeter (grid: int[,]) : int =
    let mutable ret = 0
    let rows, cols = Array2D.length1 grid, Array2D.length2 grid

    for i in 0 .. (rows - 1) do
        for j in 0 .. (cols - 1) do
            if grid.[i, j] = 1 then
                ret <- ret + 4

                if i >= 1 && grid.[i - 1, j] = 1 then
                    ret <- ret - 2

                if j >= 1 && grid.[i, j - 1] = 1 then
                    ret <- ret - 2

    ret

let grid1 =
    array2D [ [ 0; 1; 0; 0 ]; [ 1; 1; 1; 0 ]; [ 0; 1; 0; 0 ]; [ 1; 1; 0; 0 ] ]
// 16
islandPerimeter grid1

// 4
islandPerimeter (array2D [ [ 1 ] ])

// 4
islandPerimeter (array2D [ [ 1; 0 ] ])
