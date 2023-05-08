let diagonalSum (mat: int[,]) : int =
    let rec diagonalSum' i n (mat: int[,]) acc =
        if i = n then
            if n % 2 = 0 then
                acc
            else
                let half = n / 2
                acc - mat.[half, half]
        else
            let acc' = acc + mat.[i, i] + mat.[n - 1 - i, i]
            diagonalSum' (i + 1) n mat acc'

    diagonalSum' 0 (Array2D.length1 mat) mat 0

let mat1 = array2D [ [ 1; 2; 3 ]; [ 4; 5; 6 ]; [ 7; 8; 9 ] ]
// 25
diagonalSum mat1

let mat2 =
    array2D [ [ 1; 1; 1; 1 ]; [ 1; 1; 1; 1 ]; [ 1; 1; 1; 1 ]; [ 1; 1; 1; 1 ] ]
// 8
diagonalSum mat2

let mat3 = array2D [ [ 5 ] ]
// 5
diagonalSum mat3
