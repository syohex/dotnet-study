#r "nuget:FSharpx.Collections"
open FSharpx.Collections

let minimumObstacles (grid: int[,]) : int =
    let rows, cols = Array2D.length1 grid, Array2D.length2 grid
    let moves = [ (-1, 0); (0, 1); (1, 0); (0, 1) ]

    let inline isValid (r, c) =
        r >= 0 && r < rows && c >= 0 && c < cols

    let rec minimumObstacles' q (minRemoves: int[,]) =
        match PriorityQueue.tryPop q with
        | None -> minRemoves.[rows - 1, cols - 1]
        | Some((removes, (row, col)), q) ->
            let q =
                moves
                |> List.map (fun (x, y) -> row + x, col + y)
                |> List.filter isValid
                |> List.fold
                    (fun acc (r, c) ->
                        let removes = removes + grid.[r, c]

                        if removes < minRemoves.[r, c] then
                            minRemoves.[r, c] <- removes
                            PriorityQueue.insert (removes, (r, c)) acc
                        else
                            acc)
                    q

            minimumObstacles' q minRemoves

    let q = PriorityQueue.empty false |> PriorityQueue.insert (0, (0, 0))
    let minRemoves = Array2D.init rows cols (fun _ _ -> System.Int32.MaxValue)
    minimumObstacles' q minRemoves

let grid1 = array2D [ [ 0; 1; 1 ]; [ 1; 1; 0 ]; [ 1; 1; 0 ] ]
// 2
minimumObstacles grid1

let grid2 = array2D [ [ 0; 1; 0; 0; 0 ]; [ 0; 1; 0; 1; 0 ]; [ 0; 0; 0; 1; 0 ] ]
// 0
minimumObstacles grid2
