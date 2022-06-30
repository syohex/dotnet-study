open System

let minMoves2 (nums: int list) : int =
    let rec minMoves2' (nums: int64 list) (origs: int64 list) (ret: int64) =
        match nums with
        | [] -> ret |> int
        | h :: t ->
            let sum =
                origs
                |> List.fold (fun acc n -> acc + (Math.Abs(n - h))) 0L

            minMoves2' t origs (Math.Min(ret, sum))

    let nums' = nums |> List.map int64
    minMoves2' nums' nums' Int64.MaxValue


// 2
minMoves2 [ 1; 2; 3 ]

// 16
minMoves2 [ 1; 10; 2; 9 ]

// 14
minMoves2 [ 1; 0; 0; 8; 6 ]
