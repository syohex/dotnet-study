#r "nuget:FSharpx.Collections"

open FSharpx.Collections

let minTimeToReach (moveTime: int[,]) : int =
    let rows, cols = Array2D.length1 moveTime, Array2D.length2 moveTime
    let steps = [ (-1, 0); (0, -1); (1, 0); (0, 1) ]

    let rec minTimeToReach' q (dp: int[,]) =
        match PriorityQueue.tryPop q with
        | None -> dp.[rows - 1, cols - 1]
        | Some((time, row, col), q') ->
            let nexts =
                steps
                |> List.map (fun (x, y) -> row + x, col + y)
                |> List.filter (fun (x, y) -> x >= 0 && x < rows && y >= 0 && y < cols)

            let q'' =
                nexts
                |> List.fold
                    (fun acc (x, y) ->
                        let time' = (max time moveTime.[x, y]) + 1

                        if time' < dp.[x, y] then
                            dp.[x, y] <- time'
                            PriorityQueue.insert (time', x, y) acc
                        else
                            acc)
                    q'

            minTimeToReach' q'' dp

    let dp = Array2D.init rows cols (fun _ _ -> 2_000_000_000)
    let q = PriorityQueue.empty false |> PriorityQueue.insert (0, 0, 0)
    minTimeToReach' q dp

let moveTime1 = array2D [ [ 0; 4 ]; [ 4; 4 ] ]
// 6
minTimeToReach moveTime1

let moveTime2 = array2D [ [ 0; 0; 0 ]; [ 0; 0; 0 ] ]
// 3
minTimeToReach moveTime2
