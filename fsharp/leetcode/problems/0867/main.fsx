let transpose (matrix: int [,]) : int [,] =
    let rows = Array2D.length1 matrix
    let cols = Array2D.length2 matrix
    let ret = Array2D.zeroCreate cols rows

    for i in 0 .. (rows - 1) do
        for j in 0 .. (cols - 1) do
            ret.[j, i] <- matrix.[i, j]

    ret

// [[1;4;7],[2;5;8],[3;6;9]]
transpose (
    array2D [ [ 1; 2; 3 ]
              [ 4; 5; 6 ]
              [ 7; 8; 9 ] ]
)

// [[1;4],[2;5],[3;6]]
transpose (array2D [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ])
