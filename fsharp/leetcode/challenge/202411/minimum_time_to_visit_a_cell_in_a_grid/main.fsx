#r "nuget:FSharpx.Collections"

open FSharpx.Collections

let minimumTime (grid: int[,]) : int =
    let rows, cols = Array2D.length1 grid, Array2D.length2 grid
    let steps = [ (-1, 0); (0, -1); (1, 0); (0, 1) ]

    let isValid (r, c) =
        r >= 0 && r < rows && c >= 0 && c < cols

    let rec minimumTime' q (minTimes: int[,]) =
        match PriorityQueue.tryPop q with
        | None -> -1
        | Some((time, r, c), q) ->
            if r = rows - 1 && c = cols - 1 then
                time
            else
                let nexts = steps |> List.map (fun (x, y) -> r + x, y + c) |> List.filter isValid

                let q =
                    nexts
                    |> List.fold
                        (fun acc (r, c) ->
                            let wait = if (grid.[r, c] - time) % 2 = 0 then 1 else 0
                            let nextTime = max (time + 1) (grid.[r, c] + wait)

                            if nextTime < minTimes.[r, c] then
                                minTimes.[r, c] <- nextTime
                                PriorityQueue.insert (nextTime, r, c) acc
                            else
                                acc)
                        q

                minimumTime' q minTimes

    if grid.[0, 1] > 1 && grid.[1, 0] > 1 then
        -1
    else
        let q = PriorityQueue.empty false |> PriorityQueue.insert (0, 0, 0)
        let minTimes = Array2D.init rows cols (fun _ _ -> System.Int32.MaxValue)
        minimumTime' q minTimes

let grid1 = array2D [ [ 0; 1; 3; 2 ]; [ 5; 1; 2; 5 ]; [ 4; 3; 8; 6 ] ]
// 7
minimumTime grid1

let grid2 = array2D [ [ 0; 2; 4 ]; [ 3; 2; 1 ]; [ 1; 0; 4 ] ]
// -1
minimumTime grid2
