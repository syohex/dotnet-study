let equalPairs (grid: int[,]) : int =
    let n = Array2D.length1 grid

    let rows =
        seq { 0 .. (n - 1) }
        |> Seq.map (fun i -> grid.[i, *] |> Array.toList)
        |> Seq.toList

    let cols =
        seq { 0 .. (n - 1) }
        |> Seq.map (fun i -> grid.[*, i] |> Array.toList)
        |> Seq.toList

    rows
    |> List.fold (fun acc row -> acc + (cols |> List.filter (fun col -> row = col) |> List.length)) 0

let grid1 = array2D [ [ 3; 2; 1 ]; [ 1; 7; 6 ]; [ 2; 7; 7 ] ]
// 1
equalPairs grid1

let grid2 =
    array2D [ [ 3; 1; 2; 2 ]; [ 1; 4; 4; 5 ]; [ 2; 4; 2; 2 ]; [ 2; 4; 2; 2 ] ]
// 3
equalPairs grid2

let grid3 = array2D [ [ 13; 13 ]; [ 13; 13 ] ]
// 4
equalPairs grid3
