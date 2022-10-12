let maxSum (grid: int [,]) : int =
    let rows = Array2D.length1 grid
    let cols = Array2D.length2 grid

    let mutable ret = 0

    for i in 0 .. (rows - 3) do
        for j in 0 .. (cols - 3) do
            let mutable sum = 0

            for x in 0..2 do
                for y in 0..2 do
                    if not ((x = 1 && y = 0) || (x = 1 && y = 2)) then
                        sum <- sum + grid.[i + x, j + y]

            ret <- System.Math.Max(ret, sum)

    ret

let grid1 =
    array2D [ [ 6; 2; 1; 3 ]
              [ 4; 2; 1; 5 ]
              [ 9; 2; 8; 7 ]
              [ 4; 1; 2; 9 ] ]
// 30
maxSum grid1

let grid2 =
    array2D [ [ 1; 2; 3 ]
              [ 4; 5; 6 ]
              [ 7; 8; 9 ] ]
// 35
maxSum grid2
