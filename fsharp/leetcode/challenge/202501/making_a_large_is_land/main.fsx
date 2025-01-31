let largestIsland (grid: int[,]) : int =
    let rows, cols = Array2D.length1 grid, Array2D.length2 grid

    let rec setColor' row col color : int =
        grid.[row, col] <- color

        let down =
            if row >= 1 && grid.[row - 1, col] = 1 then
                setColor' (row - 1) col color
            else
                0

        let up =
            if row + 1 < rows && grid.[row + 1, col] = 1 then
                setColor' (row + 1) col color
            else
                0

        let left =
            if col >= 1 && grid.[row, col - 1] = 1 then
                setColor' row (col - 1) color
            else
                0

        let right =
            if col + 1 < cols && grid.[row, col + 1] = 1 then
                setColor' row (col + 1) color
            else
                0

        1 + down + up + left + right

    let rec setColor row col color islandSize =
        if row >= rows then
            islandSize
        elif col >= cols then
            setColor (row + 1) 0 color islandSize
        elif grid.[row, col] = 1 then
            let size = setColor' row col color
            setColor row (col + 1) (color + 1) (Map.add color size islandSize)
        else
            setColor row (col + 1) color islandSize

    let adjacentColor row col =
        let acc = Set.empty

        let acc =
            if row >= 1 && grid.[row - 1, col] >= 2 then
                Set.add grid.[row - 1, col] acc
            else
                acc

        let acc =
            if row + 1 < rows && grid.[row + 1, col] >= 2 then
                Set.add grid.[row + 1, col] acc
            else
                acc

        let acc =
            if col >= 1 && grid.[row, col - 1] >= 2 then
                Set.add grid.[row, col - 1] acc
            else
                acc

        if col + 1 < cols && grid.[row, col + 1] >= 2 then
            Set.add grid.[row, col + 1] acc
        else
            acc

    let rec largestIsland' row col islandSize acc =
        if row >= rows then
            acc
        elif col >= cols then
            largestIsland' (row + 1) 0 islandSize acc
        elif grid.[row, col] = 0 then
            let colors = adjacentColor row col
            let size = colors |> Set.fold (fun acc n -> acc + (Map.find n islandSize)) 1
            largestIsland' row (col + 1) islandSize (max acc size)
        else
            largestIsland' row (col + 1) islandSize acc

    let islandSize = setColor 0 0 2 Map.empty

    if Map.count islandSize = 0 then
        1
    elif Map.count islandSize = 1 then
        let size = Map.find 2 islandSize
        if size = rows * cols then size else size + 1
    else
        largestIsland' 0 0 islandSize 0

let grid1 = array2D [ [ 1; 0 ]; [ 0; 1 ] ]
// 3
largestIsland grid1

let grid2 = array2D [ [ 1; 1 ]; [ 1; 0 ] ]
// 4
largestIsland grid2

let grid3 = array2D [ [ 1; 1 ]; [ 1; 1 ] ]
largestIsland grid3

let grid4 =
    array2D
        [ [ 0; 0; 0; 0; 0; 0; 0 ]
          [ 0; 1; 1; 1; 1; 0; 0 ]
          [ 0; 1; 0; 0; 1; 0; 0 ]
          [ 1; 0; 1; 0; 1; 0; 0 ]
          [ 0; 1; 0; 0; 1; 0; 0 ]
          [ 0; 1; 0; 0; 1; 0; 0 ]
          [ 0; 1; 1; 1; 1; 0; 0 ] ]
// 18
largestIsland grid4
