#r "nuget:FSharpx.Collections"

open FSharpx.Collections

let minCost (grid: int[,]) : int =
    let rows, cols = Array2D.length1 grid, Array2D.length2 grid
    let steps = [ (0, 1, 1); (0, -1, 2); (1, 0, 3); (-1, 0, 4) ]

    let rec minCost' q visited =
        match PriorityQueue.tryPop q with
        | None -> failwith "never reach here"
        | Some(((cost, row, col), q')) ->
            if row = rows - 1 && col = cols - 1 then
                cost
            elif Set.contains (row, col) visited then
                minCost' q' visited
            else
                let visited = Set.add (row, col) visited

                let q' =
                    steps
                    |> List.map (fun (x, y, s) -> row + x, col + y, s)
                    |> List.filter (fun (x, y, _) -> x >= 0 && x < rows && y >= 0 && y < cols)
                    |> List.fold
                        (fun acc (x, y, sign) ->
                            let cost' = cost + if grid.[row, col] = sign then 0 else 1
                            PriorityQueue.insert (cost', x, y) acc)
                        q'

                minCost' q' visited

    let q = PriorityQueue.empty false |> PriorityQueue.insert (0, 0, 0)
    minCost' q Set.empty

let grid1 =
    array2D [ [ 1; 1; 1; 1 ]; [ 2; 2; 2; 2 ]; [ 1; 1; 1; 1 ]; [ 2; 2; 2; 2 ] ]
// 3
minCost grid1

let grid2 = array2D [ [ 1; 1; 3 ]; [ 3; 2; 2 ]; [ 1; 1; 4 ] ]
// 0
minCost grid2

let grid3 = array2D [ [ 1; 2 ]; [ 4; 3 ] ]
// 1
minCost grid3

let grid4 = array2D [ [ 4 ] ]
// 0
minCost grid4
