#r "nuget:FSharpx.Collections"

open FSharpx.Collections

let shortestPathBinaryMatrix (grid: int [,]) : int =
    let steps =
        [ (-1, -1)
          (-1, 0)
          (-1, 1)
          (0, -1)
          (0, 1)
          (1, -1)
          (1, 0)
          (1, 1) ]

    let rows = Array2D.length1 grid
    let cols = Array2D.length2 grid

    let rec shortestPathBinaryMatrix' (q: Queue<(int * int)>) (grid: int [,]) =
        match Queue.tryUncons q with
        | None -> -1
        | Some ((x, y), rest) ->
            if x = rows - 1 && y = cols - 1 then
                grid.[x, y]
            else
                let q' =
                    steps
                    |> List.fold
                        (fun acc (xStep, yStep) ->
                            let row = x + xStep
                            let col = y + yStep

                            if row >= 0
                               && row < rows
                               && col >= 0
                               && col < cols
                               && grid.[row, col] = 0 then
                                grid.[row, col] <- grid.[x, y] + 1
                                Queue.conj (row, col) acc
                            else
                                acc)
                        rest

                shortestPathBinaryMatrix' q' grid

    if grid.[0, 0] = 1 then
        -1
    else
        grid.[0, 0] <- 1
        let q = Queue.empty |> Queue.conj (0, 0)
        shortestPathBinaryMatrix' q grid

// 2
shortestPathBinaryMatrix (array2D [ [ 0; 1 ]; [ 1; 0 ] ])

// 4
shortestPathBinaryMatrix (
    array2D [ [ 0; 0; 0 ]
              [ 1; 1; 0 ]
              [ 1; 1; 0 ] ]
)

// -1
shortestPathBinaryMatrix (
    array2D [ [ 1; 0; 0 ]
              [ 1; 1; 0 ]
              [ 1; 1; 0 ] ]
)

// 7
shortestPathBinaryMatrix (
    array2D [ [ 0; 1; 0; 0; 0; 0 ]
              [ 0; 1; 1; 1; 1; 1 ]
              [ 0; 0; 0; 0; 1; 1 ]
              [ 0; 1; 0; 0; 0; 1 ]
              [ 1; 0; 0; 1; 0; 1 ]
              [ 0; 0; 1; 0; 1; 0 ] ]
)
