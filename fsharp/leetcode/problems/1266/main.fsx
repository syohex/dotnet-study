open System

let minTimeToVisitAllPoints (points: (int * int) list) =
    let rec minTimeToVisitAllPoints' (points: (int * int) list) (prevX, prevY) acc =
        match points with
        | [] -> acc
        | (x, y) :: t ->
            let dist = Math.Max(Math.Abs(x - prevX), Math.Abs(y - prevY))
            minTimeToVisitAllPoints' t (x, y) (acc + dist)

    match points with
    | [] -> failwith "never reach here"
    | _ :: [] -> 0
    | (x, y) :: t -> minTimeToVisitAllPoints' t (x, y) 0

// 7
minTimeToVisitAllPoints [ (1, 1); (3, 4); (-1, 0) ]

// 5
minTimeToVisitAllPoints [ (3, 2); (-2, 2) ]
