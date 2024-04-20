let findFarmland (island: int[,]) : (int * int * int * int) list =
    let rows, cols = Array2D.length1 island, Array2D.length2 island

    let rec findFarmland' row col =
        island.[row, col] <- 0

        let r1, c1 =
            if row + 1 < rows && island.[row + 1, col] = 1 then
                findFarmland' (row + 1) col
            else
                row, col

        let r2, c2 =
            if col + 1 < cols && island.[row, col + 1] = 1 then
                findFarmland' row (col + 1)
            else
                r1, c1

        max r1 r2, max c1 c2


    let rec f row col acc =
        if row >= rows then
            List.rev acc
        elif col >= cols then
            f (row + 1) 0 acc
        else if island.[row, col] = 0 then
            f row (col + 1) acc
        else
            let r, c = findFarmland' row col
            f row (col + 1) ((row, col, r, c) :: acc)

    f 0 0 []

let land1 = array2D [ [ 1; 0; 0 ]; [ 0; 1; 1 ]; [ 0; 1; 1 ] ]
// [(0,0,0,0),(1,1,2,2)]
findFarmland land1

let land2 = array2D [ [ 1; 1 ]; [ 1; 1 ] ]
// [(0,0,1,1)]
findFarmland land2

let land3 = array2D [ [ 0 ] ]
// []
findFarmland land3
