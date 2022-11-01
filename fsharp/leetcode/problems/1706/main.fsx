let findBall (grid: int [,]) : int list =
    let rows = Array2D.length1 grid
    let cols = Array2D.length2 grid

    let rec findBall' row col (grid: int [,]) =
        if row >= rows then
            col
        else if grid.[row, col] = 1 then
            if col = cols - 1 || grid.[row, col + 1] = -1 then
                -1
            else
                findBall' (row + 1) (col + 1) grid
        else if col = 0 || grid.[row, col - 1] = 1 then
            -1
        else
            findBall' (row + 1) (col - 1) grid

    seq { 0 .. (cols - 1) }
    |> Seq.map (fun col -> findBall' 0 col grid)
    |> Seq.toList


let grid1 =
    array2D [ [ 1; 1; 1; -1; -1 ]
              [ 1; 1; 1; -1; -1 ]
              [ -1; -1; -1; 1; 1 ]
              [ 1; 1; 1; 1; -1 ]
              [ -1; -1; -1; -1; -1 ] ]

// [1,-1,-1,-1,-1]
findBall grid1

let grid2 = array2D [ [ -1 ] ]
// [-1]
findBall grid2

let grid3 =
    array2D [ [ 1; 1; 1; 1; 1; 1 ]
              [ -1; -1; -1; -1; -1; -1 ]
              [ 1; 1; 1; 1; 1; 1 ]
              [ -1; -1; -1; -1; -1; -1 ] ]
// [0,1,2,3,4,-1]
findBall grid3
