let toMinAndMax (n: int) (buildings: (int * int) list) : (int[] * int[] * int[] * int[]) =
    let rec toMinAndMax' buildings (minRows: int[]) (maxRows: int[]) (minCols: int[]) (maxCols: int[]) =
        match buildings with
        | [] -> minRows, maxRows, minCols, maxCols
        | (row, col) :: t ->
            minRows.[col] <- min minRows.[col] row
            maxRows.[col] <- max maxRows.[col] row
            minCols.[row] <- min minCols.[row] col
            maxCols.[row] <- max maxCols.[row] col
            toMinAndMax' t minRows maxRows minCols maxCols

    let minRows = Array.init (n + 1) (fun _ -> n + 1)
    let maxRows = Array.zeroCreate (n + 1)
    let minCols = Array.init (n + 1) (fun _ -> n + 1)
    let maxCols = Array.zeroCreate (n + 1)
    toMinAndMax' buildings minRows maxRows minCols maxCols

let countCoveredBindings (n: int) (buildings: (int * int) list) =
    let minRows, maxRows, minCols, maxCols = toMinAndMax n buildings

    buildings
    |> List.filter (fun (row, col) ->
        row > minRows.[col]
        && row < maxRows.[col]
        && col > minCols.[row]
        && col < maxCols.[row])
    |> List.length

// 1
countCoveredBindings 3 [ (1, 2); (2, 2); (3, 2); (2, 1); (2, 3) ]
// 0
countCoveredBindings 3 [ (1, 1); (1, 2); (2, 1); (2, 2) ]
// 1
countCoveredBindings 5 [ (1, 3); (3, 2); (3, 3); (3, 5); (5, 3) ]
// 1
countCoveredBindings 3 [ (1, 2); (2, 1); (3, 1); (2, 3); (3, 3); (2, 2); (3, 2) ]
// 0
countCoveredBindings 3 [ (1, 2); (2, 1); (3, 1); (1, 1); (2, 3); (1, 3) ]
// 1
countCoveredBindings 4 [ (2, 4); (1, 2); (3, 1); (1, 4); (2, 3); (3, 3); (2, 2); (1, 3) ]
