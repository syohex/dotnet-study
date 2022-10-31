let isToeplitzMatrix (matrix: int [,]) : bool =
    let rec check row col v rows cols (matrix: int [,]) =
        if row >= rows || col >= cols then
            true
        else if v <> matrix.[row, col] then
            false
        else
            check (row + 1) (col + 1) v rows cols matrix

    let rows = Array2D.length1 matrix
    let cols = Array2D.length2 matrix

    let rowOK =
        seq { 0 .. (rows - 1) }
        |> Seq.forall (fun row -> check (row + 1) 1 matrix.[row, 0] rows cols matrix)

    let colOK =
        seq { 0 .. (cols - 1) }
        |> Seq.forall (fun col -> check 1 (col + 1) matrix.[0, col] rows cols matrix)

    rowOK && colOK


let matrix1 =
    array2D [ [ 1; 2; 3; 4 ]
              [ 5; 1; 2; 3 ]
              [ 9; 5; 1; 2 ] ]
// true
isToeplitzMatrix matrix1

let matrix2 = array2D [ [ 1; 2 ]; [ 2; 2 ] ]
// false
isToeplitzMatrix matrix2

let matrix3 = array2D [ [ 84 ] ]
// true
isToeplitzMatrix matrix3

let matrix4 = array2D [ [ 1; 3; 2 ]; [ 2; 1; 4 ] ]
// false
isToeplitzMatrix matrix4
