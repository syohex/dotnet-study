open System

let maxRunTime (n: int) (batteries: int list) : int64 =
    let rec maxRunTime' (n: int64) (batteries: int64 list) (left: int64) (right: int64) =
        if left >= right then
            left
        else
            let mid = right - (right - left) / 2L
            let sum' = batteries |> List.fold (fun acc n -> acc + Math.Max(mid, n)) 0L

            if sum' >= n * mid then
                maxRunTime' n batteries mid right
            else
                maxRunTime' n batteries left (mid - 1L)

    let n' = int64 n
    let batteries' = batteries |> List.map int64
    let sum = batteries' |> List.sum
    maxRunTime' n' batteries' 0L (sum / n')

// 4
maxRunTime 2 [ 3; 3; 3 ]

// 2
maxRunTime 2 [ 1; 1; 1; 1 ]
