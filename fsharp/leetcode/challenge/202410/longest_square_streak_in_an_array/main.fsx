let findSquare (n: int64) (s: Set<int64>) : int =
    let rec findSquare' n s acc =
        if Set.contains n s then
            findSquare' (n * n) s (acc + 1)
        else
            acc

    findSquare' (n * n) s 1

let longestSquareStreak (nums: int list) : int =
    let rec longestSquareStreak' nums s acc =
        match nums with
        | [] -> if acc = 1 then -1 else acc
        | h :: t ->
            let acc = max acc (findSquare h s)
            longestSquareStreak' t s acc

    let nums = nums |> List.map int64
    let s = Set.ofList nums
    longestSquareStreak' nums s 1

// 3
longestSquareStreak [ 4; 3; 6; 16; 8; 2 ]

// -1
longestSquareStreak [ 2; 3; 5; 6; 7 ]
