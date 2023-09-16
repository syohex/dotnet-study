#r "nuget:FSharpx.Collections"

open System
open FSharpx.Collections

let minimumEffortPath (heights: int[,]) : int =
    let rows, cols = Array2D.length1 heights, Array2D.length2 heights
    let steps = [ (-1, 0); (0, -1); (1, 0); (0, 1) ]

    let rec minimumEffortPath' (q: IPriorityQueue<(int * int * int)>) (dp: int[,]) =
        match PriorityQueue.tryPop q with
        | None -> dp.[rows - 1, cols - 1]
        | Some((maxDiff, row, col), q') ->
            let q'' =
                steps
                |> List.map (fun (x, y) -> row + x, col + y)
                |> List.filter (fun (x, y) -> x >= 0 && x < rows && y >= 0 && y < cols)
                |> List.fold
                    (fun acc (x, y) ->
                        let diff = Math.Abs(heights.[row, col] - heights.[x, y])
                        let maxDiff' = Math.Max(diff, maxDiff)

                        if maxDiff' < dp.[x, y] then
                            dp.[x, y] <- maxDiff'
                            PriorityQueue.insert (maxDiff', x, y) acc
                        else
                            acc)
                    q'

            minimumEffortPath' q'' dp

    let dp = Array2D.init rows cols (fun _ _ -> Int32.MaxValue)
    dp.[0, 0] <- 0

    let q = PriorityQueue.empty false |> PriorityQueue.insert (0, 0, 0)
    minimumEffortPath' q dp

let heights1 = array2D [ [ 1; 2; 3 ]; [ 3; 8; 2 ]; [ 5; 3; 5 ] ]
// 2
minimumEffortPath heights1

let heights2 = array2D [ [ 1; 2; 3 ]; [ 3; 8; 4 ]; [ 5; 3; 5 ] ]
// 1
minimumEffortPath heights2

let heights3 =
    array2D
        [ [ 1; 2; 1; 1; 1 ]
          [ 1; 2; 1; 2; 1 ]
          [ 1; 2; 1; 2; 1 ]
          [ 1; 2; 1; 2; 1 ]
          [ 1; 1; 1; 2; 1 ] ]
// 0
minimumEffortPath heights3

let heights4 = array2D [ [ 3 ] ]
// 0
minimumEffortPath heights4

let heights5 = array2D [ [ 1; 10; 5; 4; 3; 2; 1 ] ]
// 9
minimumEffortPath heights5
