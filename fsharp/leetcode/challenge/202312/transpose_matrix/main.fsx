let transpose (matrix: int[,]) : int[,] =
    let rows, cols = Array2D.length1 matrix, Array2D.length2 matrix

    let rec transpose' i j (ret: int[,]) =
        if i >= cols then
            ret
        elif j >= rows then
            transpose' (i + 1) 0 ret
        else
            ret.[i, j] <- matrix.[j, i]
            transpose' i (j + 1) ret

    let ret = Array2D.zeroCreate cols rows
    transpose' 0 0 ret

let mat1 = array2D [ [ 1; 2; 3 ]; [ 4; 5; 6 ]; [ 7; 8; 9 ] ]
// [[1,4,7];[2,5,8];[3,6,9]]
transpose mat1

let mat2 = array2D [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ]
// [[1,4];[2,5];[3,6]]
transpose mat2
