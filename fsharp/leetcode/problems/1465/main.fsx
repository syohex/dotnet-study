let maxArea (h: int) (w: int) (horizontal_cuts: int list) (vertical_cuts: int list) : int =
    let rec maxLen nums prev (ret: int) =
        match nums with
        | [] -> ret |> int64
        | h :: t ->
            let ret' = System.Math.Max(ret, h - prev)
            maxLen t h ret'

    let hCuts = (0 :: h :: horizontal_cuts) |> List.sort
    let vCuts = (0 :: w :: vertical_cuts) |> List.sort

    let hMax = maxLen hCuts.Tail hCuts.Head 0
    let vMax = maxLen vCuts.Tail vCuts.Head 0

    ((hMax * vMax) % 1_000_000_007L) |> int

// 4
maxArea 5 4 [ 1; 2; 4 ] [ 1; 3 ]

// 6
maxArea 5 4 [ 3; 1 ] [ 1 ]

// 9
maxArea 5 4 [ 3 ] [ 3 ]
