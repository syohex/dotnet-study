let numIslands (grid: char[,]) : int =
    let rows, cols = Array2D.length1 grid, Array2D.length2 grid

    let rec fillIsland row col =
        grid.[row, col] <- '0'

        if row >= 1 && grid.[row - 1, col] = '1' then
            fillIsland (row - 1) col

        if row + 1 < rows && grid.[row + 1, col] = '1' then
            fillIsland (row + 1) col

        if col >= 1 && grid.[row, col - 1] = '1' then
            fillIsland row (col - 1)

        if col + 1 < cols && grid.[row, col + 1] = '1' then
            fillIsland row (col + 1)

    let mutable ret = 0

    for i in 0 .. (rows - 1) do
        for j in 0 .. (cols - 1) do
            if grid.[i, j] = '1' then
                ret <- ret + 1
                fillIsland i j

    ret

let grid1 =
    array2D
        [ [ '1'; '1'; '1'; '1'; '0' ]
          [ '1'; '1'; '0'; '1'; '0' ]
          [ '1'; '1'; '0'; '0'; '0' ]
          [ '0'; '0'; '0'; '0'; '0' ] ]
// 1
numIslands grid1

let grid2 =
    array2D
        [ [ '1'; '1'; '0'; '0'; '0' ]
          [ '1'; '1'; '0'; '0'; '0' ]
          [ '0'; '0'; '1'; '0'; '0' ]
          [ '0'; '0'; '0'; '1'; '1' ] ]
// 3
numIslands grid2
