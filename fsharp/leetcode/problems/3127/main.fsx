let canMakeSquare (grid: char[,]) : bool =
    let rows, cols = Array2D.length1 grid, Array2D.length2 grid

    let rec countColor i j row col whites blacks =
        if i >= 2 then
            whites >= 3 || blacks >= 3
        elif j >= 2 then
            countColor (i + 1) 0 row col whites blacks
        else
            let j' = j + 1

            if grid.[row + i, col + j] = 'W' then
                countColor i j' row col (whites + 1) blacks
            else
                countColor i j' row col whites blacks

    let rec canMakeSquare' row col =
        if row >= rows - 1 then false
        elif col >= cols - 1 then canMakeSquare' (row + 1) 0
        else if countColor 0 0 row col 0 0 then true
        else canMakeSquare' row (col + 1)

    canMakeSquare' 0 0

// true
canMakeSquare (array2D [ [ 'B'; 'W'; 'B' ]; [ 'B'; 'W'; 'W' ]; [ 'B'; 'W'; 'B' ] ])
// false
canMakeSquare (array2D [ [ 'B'; 'W'; 'B' ]; [ 'W'; 'B'; 'W' ]; [ 'B'; 'W'; 'B' ] ])
// true
canMakeSquare (array2D [ [ 'B'; 'W'; 'B' ]; [ 'B'; 'W'; 'W' ]; [ 'B'; 'W'; 'W' ] ])
