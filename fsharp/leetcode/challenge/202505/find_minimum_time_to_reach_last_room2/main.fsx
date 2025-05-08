#r "nuget:FSharpx.Collections"

open FSharpx.Collections

let minTimeToReach (moveTime: int[,]) : int =
    let rows, cols = Array2D.length1 moveTime, Array2D.length2 moveTime
    let steps = [ (-1, 0); (0, -1); (1, 0); (0, 1) ]

    let rec minTimeToReach' q (minTimes: int[,]) =
        match PriorityQueue.tryPop q with
        | None -> minTimes.[rows - 1, cols - 1]
        | Some(((curTime, row, col, wait), q')) ->
            let nexts =
                steps
                |> List.map (fun (x, y) -> row + x, col + y)
                |> List.filter (fun (x, y) -> x >= 0 && x < rows && y >= 0 && y < cols)

            let q'' =
                nexts
                |> List.fold
                    (fun acc (r, c) ->
                        let time = (max curTime moveTime.[r, c]) + wait

                        if time < minTimes.[r, c] then
                            minTimes.[r, c] <- time
                            let wait' = if wait = 1 then 2 else 1
                            PriorityQueue.insert (time, r, c, wait') acc
                        else
                            acc)
                    q'

            minTimeToReach' q'' minTimes

    let minTimes = Array2D.init rows cols (fun _ _ -> 1_100_000_000)
    let q = PriorityQueue.empty false |> PriorityQueue.insert (0, 0, 0, 1)
    minTimeToReach' q minTimes

let moveTime1 = array2D [ [ 0; 4 ]; [ 4; 4 ] ]
// 7
minTimeToReach moveTime1

let moveTime2 = array2D [ [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ] ]
// 6
minTimeToReach moveTime2
