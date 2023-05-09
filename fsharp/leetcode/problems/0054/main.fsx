type State =
    | Right
    | Down
    | Left
    | Up

let spiralOrder (matrix: int[,]) : int list =
    let rows = Array2D.length1 matrix
    let cols = Array2D.length2 matrix
    let visited = 101

    let rec spiralOrder' row col steps state acc =
        if steps >= rows * cols then
            List.rev acc
        else
            let acc' = matrix.[row, col] :: acc
            matrix.[row, col] <- visited
            let steps' = steps + 1

            match state with
            | Right ->
                if col + 1 < cols && matrix.[row, col + 1] <> visited then
                    spiralOrder' row (col + 1) steps' state acc'
                else
                    spiralOrder' (row + 1) col steps' Down acc'
            | Down ->
                if row + 1 < rows && matrix.[row + 1, col] <> visited then
                    spiralOrder' (row + 1) col steps' state acc'
                else
                    spiralOrder' row (col - 1) steps' Left acc'
            | Left ->
                if col - 1 >= 0 && matrix.[row, col - 1] <> visited then
                    spiralOrder' row (col - 1) steps' state acc'
                else
                    spiralOrder' (row - 1) col steps' Up acc'
            | Up ->
                if row - 1 >= 0 && matrix.[row - 1, col] <> visited then
                    spiralOrder' (row - 1) col steps' state acc'
                else
                    spiralOrder' row (col + 1) steps' Right acc'

    spiralOrder' 0 0 0 Right []

let matrix1 = array2D [ [ 1; 2; 3 ]; [ 4; 5; 6 ]; [ 7; 8; 9 ] ]
// [1,2,3,6,9,8,7,4,5]
spiralOrder matrix1

let matrix2 = array2D [ [ 1; 2; 3; 4 ]; [ 5; 6; 7; 8 ]; [ 9; 10; 11; 12 ] ]
// [1,2,3,4,8,12,11,10,9,5,6,7]
spiralOrder matrix2

let matrix3 = array2D [ [ 5 ] ]
// [5]
spiralOrder matrix3
let matrix4 = array2D [ [ 1; 2; 3; 4 ] ]
// [1,2,3,4]
spiralOrder matrix4
