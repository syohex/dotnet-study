let numEnclaves (grid: int[,]) : int =
    let rows, cols = Array2D.length1 grid, Array2D.length2 grid
    let steps = [ (-1, 0); (0, -1); (1, 0); (0, 1) ]

    let rec dfs row col =
        if row < 0 || row >= rows || col < 0 || col >= cols then
            None
        elif grid.[row, col] = 0 then
            Some(0)
        else
            grid.[row, col] <- 0

            steps
            |> List.fold
                (fun acc (x, y) ->
                    let r, c = row + x, col + y
                    match dfs r c, acc with
                    | _, None -> None
                    | None, _ -> None
                    | Some(v), Some(w) -> Some(v + w))
                (Some(1))

    let mutable ret = 0

    for i in 0 .. (rows - 1) do
        for j in 0 .. (cols - 1) do
            if grid.[i, j] = 1 then
                match dfs i j with
                | None -> ret <- ret + 0
                | Some(v) ->
                    ret <- ret + v

    ret

let grid1 =
    array2D [ [ 0; 0; 0; 0 ]; [ 1; 0; 1; 0 ]; [ 0; 1; 1; 0 ]; [ 0; 0; 0; 0 ] ]
// 3
numEnclaves grid1

let grid2 =
    array2D [ [ 0; 1; 1; 0 ]; [ 0; 0; 1; 0 ]; [ 0; 0; 1; 0 ]; [ 0; 0; 0; 0 ] ]
// 0
numEnclaves grid2

let grid3 =
    array2D
        [ [ 0; 0; 0; 1; 1; 1; 0; 1; 0; 0 ]
          [ 1; 1; 0; 0; 0; 1; 0; 1; 1; 1 ]
          [ 0; 0; 0; 1; 1; 1; 0; 1; 0; 0 ]
          [ 0; 1; 1; 0; 0; 0; 1; 0; 1; 0 ]
          [ 0; 1; 1; 1; 1; 1; 0; 0; 1; 0 ]
          [ 0; 0; 1; 0; 1; 1; 1; 1; 0; 1 ]
          [ 0; 1; 1; 0; 0; 0; 1; 1; 1; 1 ]
          [ 0; 0; 1; 0; 0; 1; 0; 1; 0; 1 ]
          [ 1; 0; 1; 0; 1; 1; 0; 0; 0; 0 ]
          [ 0; 0; 0; 0; 1; 1; 0; 0; 0; 1 ] ]
// 3
numEnclaves grid3
