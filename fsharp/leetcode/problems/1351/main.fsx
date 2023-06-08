let countNegatives (grid: int[,]) : int =
    let rows = Array2D.length1 grid
    let cols = Array2D.length2 grid

    let rec upperBound (row: int[]) left right =
        if left > right then
            left
        else
            let mid = left + (right - left) / 2

            if row.[mid] < 0 then
                upperBound row left (mid - 1)
            else
                upperBound row (mid + 1) right

    let rec countNegatives' i (grid: int[,]) acc =
        if i >= rows then
            acc
        else
            let negatives = cols - (upperBound grid.[i, *] 0 (cols - 1))
            let acc' = acc + negatives
            countNegatives' (i + 1) grid acc'

    countNegatives' 0 grid 0

let grid1 =
    array2D [ [ 4; 3; 2; -1 ]; [ 3; 2; 1; -1 ]; [ 1; 1; -1; -2 ]; [ -1; -1; -2; -3 ] ]
// 8
countNegatives grid1

let grid2 = array2D [ [ 3; 2 ]; [ 1; 0 ] ]
// 0
countNegatives grid2
