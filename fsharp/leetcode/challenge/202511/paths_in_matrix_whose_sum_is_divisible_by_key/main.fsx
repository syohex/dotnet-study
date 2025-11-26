let numberOfPaths (grid: int[,]) (k: int) : int =
    let rows, cols = Array2D.length1 grid, Array2D.length2 grid
    let modulo = 1_000_000_007

    let getValue row col m dp =
        dp |> Map.tryFind (row, col, m) |> Option.defaultValue 0

    let rec numberOfPaths' row col dp =
        if row >= rows then
            Map.find (rows - 1, cols - 1, 0) dp
        elif col >= cols then
            numberOfPaths' (row + 1) 0 dp
        else
            let m = grid.[row, col] % k

            let dp =
                seq { 0 .. (k - 1) }
                |> Seq.fold
                    (fun acc i ->
                        let prev = (i + k - m) % k
                        let v1 = getValue (row - 1) col prev acc
                        let v2 = getValue row (col - 1) prev acc
                        let v = (v1 + v2) % modulo
                        Map.add (row, col, i) v acc)
                    dp

            numberOfPaths' row (col + 1) dp

    let dp = Map.empty |> Map.add (0, 0, grid.[0, 0] % k) 1
    numberOfPaths' 0 1 dp

let grid1 = array2D [ [ 5; 2; 4 ]; [ 3; 0; 5 ]; [ 0; 7; 2 ] ]
// 2
numberOfPaths grid1 3

let grid2 = array2D [ [ 0; 0 ] ]
// 1
numberOfPaths grid2 5

let grid3 = array2D [ [ 7; 3; 4; 9 ]; [ 2; 3; 6; 2 ]; [ 2; 3; 7; 0 ] ]
// 10
numberOfPaths grid3 1
