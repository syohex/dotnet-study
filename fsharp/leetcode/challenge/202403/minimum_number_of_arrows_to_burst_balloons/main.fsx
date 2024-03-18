let findMinArrowShots (points: (int * int) list) : int =
    let rec findMinArrowsShots' points endPoint ret =
        match points with
        | [] -> ret
        | (s, e) :: t ->
            if endPoint < s then
                findMinArrowsShots' t e (ret + 1)
            else
                findMinArrowsShots' t endPoint ret

    let points' = List.sortBy snd points
    findMinArrowsShots' points' (List.head points' |> snd) 1

// 2
findMinArrowShots [ (10, 16); (2, 8); (1, 6); (7, 12) ]

//4
findMinArrowShots [ (1, 2); (3, 4); (5, 6); (7, 8) ]

// 2
findMinArrowShots [ (1, 2); (2, 3); (3, 4); (4, 5) ]
