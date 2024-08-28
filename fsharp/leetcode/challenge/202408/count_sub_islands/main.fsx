let countSubIslands (grid1: int[,]) (grid2: int[,]) : int =
    let rows, cols = Array2D.length1 grid1, Array2D.length2 grid2
    let steps = [ (-1, 0); (0, -1); (1, 0); (0, 1) ]

    let isValid (x, y) =
        x >= 0 && x < rows && y >= 0 && y < cols

    let rec isSubIsland q ok =
        match q with
        | [] -> ok
        | _ ->
            // mark visited
            q |> List.iter (fun (x, y) -> grid2.[x, y] <- 0)

            let ok =
                if ok then
                    q |> List.exists (fun (x, y) -> grid1.[x, y] = 0) |> not
                else
                    false

            let q =
                q
                |> List.fold
                    (fun acc (x, y) ->
                        let nexts =
                            steps
                            |> List.map (fun (r, c) -> x + r, y + c)
                            |> List.filter isValid
                            |> List.filter (fun (x, y) -> grid2.[x, y] = 1)

                        nexts @ acc)
                    []

            isSubIsland q ok

    let rec countSubIslands' row col acc =
        if row >= rows then
            acc
        elif col >= cols then
            countSubIslands' (row + 1) 0 acc
        else if grid2.[row, col] = 1 && isSubIsland [ (row, col) ] true then
            countSubIslands' row (col + 1) (acc + 1)
        else
            countSubIslands' row (col + 1) acc

    countSubIslands' 0 0 0

let grid11 =
    array2D
        [ [ 1; 1; 1; 0; 0 ]
          [ 0; 1; 1; 1; 1 ]
          [ 0; 0; 0; 0; 0 ]
          [ 1; 0; 0; 0; 0 ]
          [ 1; 1; 0; 1; 1 ] ]

let grid12 =
    array2D
        [ [ 1; 1; 1; 0; 0 ]
          [ 0; 0; 1; 1; 1 ]
          [ 0; 1; 0; 0; 0 ]
          [ 1; 0; 1; 1; 0 ]
          [ 0; 1; 0; 1; 0 ] ]

// 3
countSubIslands grid11 grid12

let grid21 =
    array2D
        [ [ 1; 0; 1; 0; 1 ]
          [ 1; 1; 1; 1; 1 ]
          [ 0; 0; 0; 0; 0 ]
          [ 1; 1; 1; 1; 1 ]
          [ 1; 0; 1; 0; 1 ] ]

let grid22 =
    array2D
        [ [ 0; 0; 0; 0; 0 ]
          [ 1; 1; 1; 1; 1 ]
          [ 0; 1; 0; 1; 0 ]
          [ 0; 1; 0; 1; 0 ]
          [ 1; 0; 0; 0; 1 ] ]

// 2
countSubIslands grid21 grid22
