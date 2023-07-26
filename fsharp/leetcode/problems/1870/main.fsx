open System

let minSpeedOnTime (dist: int list) (hour: double) : int =
    let totalTime speed dist len =
        dist
        |> List.indexed
        |> List.fold
            (fun acc (i, (d: double)) ->
                let time = d / speed
                if i = len - 1 then acc + time else acc + Math.Ceiling(time))
            0.0

    let rec minSpeedOnTime' left right dist len hour minSpeed =
        if left > right then
            minSpeed
        else
            let mid = left + (right - left) / 2
            let total = totalTime mid dist len

            if total <= hour then
                minSpeedOnTime' left (mid - 1) dist len hour mid
            else
                minSpeedOnTime' (mid + 1) right dist len hour minSpeed

    let dist' = List.map double dist
    let len = List.length dist
    minSpeedOnTime' 0 10_000_000 dist' len hour -1

// 1
minSpeedOnTime [ 1; 3; 2 ] 6

// 3
minSpeedOnTime [ 1; 3; 2 ] 2.7

// -1
minSpeedOnTime [ 1; 3; 2 ] 1.9
