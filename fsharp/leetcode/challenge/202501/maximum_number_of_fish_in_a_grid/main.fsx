let findMaxFish (grid: int[,]) : int =
    let rows, cols = Array2D.length1 grid, Array2D.length2 grid

    let rec collectFish row col (grid: int[,]) =
        let current = grid.[row, col]
        grid.[row, col] <- 0

        let left =
            if col >= 1 && grid.[row, col - 1] <> 0 then
                collectFish row (col - 1) grid
            else
                0

        let up =
            if row >= 1 && grid.[row - 1, col] <> 0 then
                collectFish (row - 1) col grid
            else
                0

        let right =
            if col + 1 < cols && grid.[row, col + 1] <> 0 then
                collectFish row (col + 1) grid
            else
                0

        let down =
            if row + 1 < rows && grid.[row + 1, col] <> 0 then
                collectFish (row + 1) col grid
            else
                0

        current + left + up + right + down

    let rec findMaxFish' row col (grid: int[,]) acc =
        if row >= rows then
            acc
        elif col >= cols then
            findMaxFish' (row + 1) 0 grid acc
        elif grid.[row, col] <> 0 then
            let acc = max acc (collectFish row col grid)
            findMaxFish' row (col + 1) grid acc
        else
            findMaxFish' row (col + 1) grid acc

    findMaxFish' 0 0 grid 0

let grid1 =
    array2D [ [ 0; 2; 1; 0 ]; [ 4; 0; 0; 3 ]; [ 1; 0; 0; 4 ]; [ 0; 3; 2; 0 ] ]
// 7
findMaxFish grid1

let grid2 =
    array2D [ [ 1; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 1 ] ]
// 1
findMaxFish grid2
