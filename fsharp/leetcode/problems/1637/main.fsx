open System

let maxWidthOfVerticalArea (points: (int * int) list) : int =
    points
    |> List.sort
    |> List.windowed 2
    |> List.fold
        (fun acc ps ->
            match ps with
            | (x1, _) :: (x2, _) :: [] -> Math.Max(acc, x2 - x1)
            | _ -> failwith "never reach here")
        0

// 1
maxWidthOfVerticalArea [ (8, 7); (9, 9); (7, 4); (9, 7) ]

// 3
maxWidthOfVerticalArea [ (3, 1); (0, 9); (1, 0); (1, 4); (5, 3); (8, 8) ]
