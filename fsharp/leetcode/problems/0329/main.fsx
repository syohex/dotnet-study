open System

let longestIncreasingPath (matrix: int [,]) : int =
    let steps = [ (-1, 0); (0, -1); (1, 0); (0, 1) ]

    let rows = Array2D.length1 matrix
    let cols = Array2D.length2 matrix

    let validNextPositions row col (matrix: int [,]) =
        steps
        |> List.map (fun (r, c) -> row + r, col + c)
        |> List.filter (fun (r, c) ->
            r >= 0
            && r < rows
            && c >= 0
            && c < cols
            && matrix.[r, c] > matrix.[row, col])

    let rec longestIncreasingPath' row col matrix (cache: Map<(int * int), int>) =
        match Map.tryFind (row, col) cache with
        | Some (v) -> v, cache
        | None ->
            let ret, cache' =
                validNextPositions row col matrix
                |> List.fold
                    (fun (ret, cache) (r, c) ->
                        let r, cache' = longestIncreasingPath' r c matrix cache
                        Math.Max(r, ret), cache')
                    (0, cache)

            ret + 1, Map.add (row, col) (ret + 1) cache'

    let points =
        seq {
            for i in 0 .. (rows - 1) do
                for j in 0 .. (cols - 1) do
                    yield (i, j)
        }

    points
    |> Seq.fold
        (fun (acc, cache) (r, c) ->
            let ret, cache' = longestIncreasingPath' r c matrix cache
            Math.Max(acc, ret), cache')
        (0, Map.empty)
    |> fst

let matrix1 =
    array2D [ [ 9; 9; 4 ]
              [ 6; 6; 8 ]
              [ 2; 1; 1 ] ]
// 4
longestIncreasingPath matrix1

let matrix2 =
    array2D [ [ 3; 4; 5 ]
              [ 3; 2; 6 ]
              [ 2; 2; 1 ] ]
// 4
longestIncreasingPath matrix2

let matrix3 = array2D [ [ 1 ] ]
// 1
longestIncreasingPath matrix3
