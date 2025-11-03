let minCost (colors: string) (neededTime: int list) : int =
    let rec minCost' ct prev maxVal sum acc =
        match ct with
        | [] -> acc + (sum - maxVal)
        | (time, color) :: t ->
            if color = prev then
                minCost' t prev (max time maxVal) (sum + time) acc
            else
                minCost' t color time time (acc + sum - maxVal)


    let ct = colors |> Seq.toList |> List.zip neededTime
    minCost' ct '?' 0 0 0

// 3
minCost "abaac" [ 1; 2; 3; 4; 5 ]

// 0
minCost "abc" [ 1; 2; 3 ]

// 2
minCost "aabaa" [ 1; 2; 3; 4; 1 ]

// 23
minCost "bbbaaa" [ 4; 9; 3; 8; 8; 9 ]
