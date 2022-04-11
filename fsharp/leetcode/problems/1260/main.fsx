let toOneDimention (grid: int [,]) (cells: int) (cols: int) : int list =
    let rec toOneDimention' i (grid: int [,]) cells cols acc =
        if i = cells then
            acc |> List.rev
        else
            let row = i / cols
            let col = i % cols
            let v = grid.[row, col]
            toOneDimention' (i + 1) grid cells cols (v :: acc)

    toOneDimention' 0 grid cells cols []

let shift (grid1: int list) (k: int) : int list =
    let len = grid1.Length

    (List.skip (len - k) grid1)
    @ (List.take (len - k) grid1)

let shiftGrid (grid: int [,]) (k: int) : int [,] =
    let rec shiftGrid' i cols grid1 (ret: int [,]) =
        match grid1 with
        | [] -> ret
        | h :: t ->
            let row = i / cols
            let col = i % cols
            ret.[row, col] <- h
            shiftGrid' (i + 1) cols t ret

    let rows = Array2D.length1 grid
    let cols = Array2D.length2 grid
    let cells = rows * cols
    let grid1 = toOneDimention grid cells cols
    let grid1' = shift grid1 (k % cells)

    let ret = Array2D.zeroCreate<int> rows cols
    shiftGrid' 0 cols grid1' ret

let grid1 =
    array2D [ [ 1; 2; 3 ]
              [ 4; 5; 6 ]
              [ 7; 8; 9 ] ]

// [[9,1,2],[4,5,6],[7,8,9]]
shiftGrid grid1 1

let grid2 =
    array2D [ [ 3; 8; 1; 9 ]
              [ 19; 7; 2; 5 ]
              [ 4; 6; 11; 10 ]
              [ 12; 0; 21; 13 ] ]

// [[12,0,21,13],[3,8,1,9],[19,7,2,5],[4,6,11,10]]
shiftGrid grid2 4

let grid3 =
    array2D [ [ 1; 2; 3 ]
              [ 4; 5; 6 ]
              [ 7; 8; 9 ] ]

// [[1,2,3],[4,5,6],[7,8,9]]
shiftGrid grid3 9

let grid4 =
    array2D [ [ 1 ]
              [ 2 ]
              [ 3 ]
              [ 4 ]
              [ 7 ]
              [ 6 ]
              [ 5 ] ]

// [[6],[5],[1],[2],[3],[4],[7]]
shiftGrid grid4 23
