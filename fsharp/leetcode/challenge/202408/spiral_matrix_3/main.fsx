let spiralMatrixIII (rows: int) (cols: int) (rStart: int) (cStart: int) : (int * int) list =
    let steps = [| (0, 1); (1, 0); (0, -1); (-1, 0) |]
    let limit = rows * cols

    let rec spiralMatrixIII' row col direction step acc =
        if List.length acc >= limit then
            List.rev acc
        else
            let (acc, row, col, direction) =
                seq { 1..2 }
                |> Seq.fold
                    (fun (acc, row, col, direction) _ ->
                        let (acc, row, col) =
                            seq { 0 .. (step - 1) }
                            |> Seq.fold
                                (fun (acc, row, col) _ ->
                                    let x, y = steps.[direction]

                                    if row >= 0 && row < rows && col >= 0 && col < cols then
                                        (row, col) :: acc, row + x, col + y
                                    else
                                        acc, row + x, col + y)
                                (acc, row, col)

                        acc, row, col, (direction + 1) % 4)
                    (acc, row, col, direction)

            spiralMatrixIII' row col direction (step + 1) acc

    spiralMatrixIII' rStart cStart 0 1 []

// [[0,0],[0,1],[0,2],[0,3]]
spiralMatrixIII 1 4 0 0

// [
//   [1,4],[1,5],[2,5],[2,4],[2,3],[1,3],[0,3],[0,4],[0,5],[3,5],[3,4],[3,3],[3,2],[2,2],[1,2]
//   [0,2],[4,5],[4,4],[4,3],[4,2],[4,1],[3,1],[2,1],[1,1],[0,1],[4,0],[3,0],[2,0],[1,0],[0,0]
// ]
spiralMatrixIII 5 6 1 4
