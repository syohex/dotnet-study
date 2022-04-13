type Direction =
    | Right
    | Down
    | Left
    | Up

let generateMatrix (n: int) : int [,] =
    let rec generateMatrix' (matrix: int [,]) i n row col dir =
        if i > (n * n) then
            matrix
        else
            matrix.[row, col] <- i

            let row', col', dir' =
                match dir with
                | Right ->
                    if col + 1 <= n - 1 && matrix.[row, col + 1] = -1 then
                        row, col + 1, dir
                    else
                        row + 1, col, Down
                | Down ->
                    if row + 1 <= n - 1 && matrix.[row + 1, col] = -1 then
                        row + 1, col, dir
                    else
                        row, col - 1, Left
                | Left ->
                    if col >= 1 && matrix.[row, col - 1] = -1 then
                        row, col - 1, dir
                    else
                        row - 1, col, Up
                | Up ->
                    if row >= 1 && matrix.[row - 1, col] = -1 then
                        row - 1, col, dir
                    else
                        row, col + 1, Right

            generateMatrix' matrix (i + 1) n row' col' dir'

    let matrix = Array2D.create n n -1
    generateMatrix' matrix 1 n 0 0 Right

// [[1,2,3],[8,9,4],[7,6,5]]
generateMatrix 3

// [[1]]
generateMatrix 1

// [[1,2,3,4],[12,13,14,4],[11,16,15,6],[10,9,8,7]]
generateMatrix 4
