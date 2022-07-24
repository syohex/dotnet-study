let searchMatrix (matrix: int [,]) (target: int) : bool =
    let binarySearch pos isRow target (matrix: int [,]) =
        let rec binarySearch' left right pos isRow target (matrix: int [,]) =
            if left > right then
                false
            else
                let index = (left + right) / 2

                let mid =
                    if isRow then
                        matrix.[pos, index]
                    else
                        matrix.[index, pos]

                if target = mid then
                    true
                elif target < mid then
                    binarySearch' left (index - 1) pos isRow target matrix
                else
                    binarySearch' (index + 1) right pos isRow target matrix

        let right =
            if isRow then
                (Array2D.length1 matrix) - 1
            else
                (Array2D.length2 matrix) - 1

        binarySearch' 0 right pos isRow target matrix


    let rec searchMatrix' i limit (matrix: int [,]) =
        if i >= limit then
            false
        else
            let rowFound = binarySearch i true target matrix
            let colFound = binarySearch i false target matrix

            if rowFound || colFound then
                true
            else
                searchMatrix' (i + 1) limit matrix

    let limit = System.Math.Min(Array2D.length1 matrix, Array2D.length2 matrix)
    searchMatrix' 0 limit matrix

let matrix1 =
    array2D [ [ 1; 4; 7; 11; 15 ]
              [ 2; 5; 8; 12; 19 ]
              [ 3; 6; 9; 16; 22 ]
              [ 10; 13; 14; 17; 24 ]
              [ 18; 21; 23; 26; 30 ] ]
// true
searchMatrix matrix1 5
// false
searchMatrix matrix1 20
