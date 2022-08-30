let rotate (matrix: int [,]) : unit =
    let n = Array2D.length1 matrix

    for i in 0 .. (n / 2 - 1) do
        for j in 0 .. (n - 1) do
            let tmp = matrix.[i, j]
            matrix.[i, j] <- matrix.[n - 1 - i, j]
            matrix.[n - 1 - i, j] <- tmp

    for i in 0 .. (n - 1) do
        for j in (i + 1) .. (n - 1) do
            let tmp = matrix.[i, j]
            matrix.[i, j] <- matrix.[j, i]
            matrix.[j, i] <- tmp

    ()

let matrix1 =
    array2D [ [ 1; 2; 3 ]
              [ 4; 5; 6 ]
              [ 7; 8; 9 ] ]

rotate matrix1

// [ [ 7, 4, 1 ], [ 8, 5, 2 ], [ 9, 6, 3 ] ]
matrix1

let matrix2 =
    array2D [ [ 5; 1; 9; 11 ]
              [ 2; 4; 8; 10 ]
              [ 13; 3; 6; 7 ]
              [ 15; 14; 12; 16 ] ]

rotate matrix2

// [[15,13,2,5],[14,3,4,1],[12,6,8,9],[16,7,10,11]]
matrix2
