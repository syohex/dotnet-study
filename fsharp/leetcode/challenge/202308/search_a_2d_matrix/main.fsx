let searchMatrix (matrix: int[,]) (target: int) : bool =
    let rec searchMatrix' (matrix: int[,]) left right cols target =
        if left > right then
            false
        else
            let mid = left + (right - left) / 2
            let v = matrix.[mid / cols, mid % cols]

            if v = target then
                true
            elif v > target then
                searchMatrix' matrix left (mid - 1) cols target
            else
                searchMatrix' matrix (mid + 1) right cols target

    let rows, cols = Array2D.length1 matrix, Array2D.length2 matrix
    searchMatrix' matrix 0 (rows * cols - 1) cols target

let matrix1 = array2D [ [ 1; 3; 5; 7 ]; [ 10; 11; 16; 20 ]; [ 23; 30; 34; 60 ] ]
// true
searchMatrix matrix1 3
// false
searchMatrix matrix1 13

let matrix2 = array2D [ [ 1 ] ]
// false
searchMatrix matrix2 0
