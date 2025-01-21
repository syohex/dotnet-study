let gridGame (grid: int[,]) : int64 =
    let cols = Array2D.length2 grid

    let rec gridGame' col sum1 sum2 acc =
        if col >= cols then
            acc
        else
            let sum1 = sum1 - int64 grid.[0, col]
            let acc = min acc (max sum1 sum2)
            gridGame' (col + 1) sum1 (sum2 + int64 grid.[1, col]) acc

    let sum1 = grid.[0, *] |> Seq.sum |> int64
    gridGame' 0 sum1 0 System.Int64.MaxValue

let grid1 = array2D [ [ 2; 5; 4 ]; [ 1; 5; 1 ] ]
// 4
gridGame grid1

let grid2 = array2D [ [ 3; 3; 1 ]; [ 8; 5; 2 ] ]
// 4
gridGame grid2

let grid3 = array2D [ [ 1; 3; 1; 15 ]; [ 1; 3; 3; 1 ] ]
// 7
gridGame grid3
