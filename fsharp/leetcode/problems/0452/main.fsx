let findMinArrowShots (points: (int * int) list) : int =
    let rec findMinArrowShots' points (end1: int) acc =
        match points with
        | [] -> acc
        | (s, e) :: t ->
            if s <= end1 then
                findMinArrowShots' t (System.Math.Min(e, end1)) acc
            else
                findMinArrowShots' t e (acc + 1)

    let points = points |> List.sortWith (fun (s1, _) (s2, _) -> compare s1 s2)
    findMinArrowShots' (points.Tail) (snd points.Head) 1

// 2
findMinArrowShots [ (10, 16); (2, 8); (1, 6); (7, 12) ]

// 4
findMinArrowShots [ (1, 2); (3, 4); (5, 6); (7, 8) ]

// 2
findMinArrowShots [ (1, 2); (2, 3); (3, 4); (4, 5) ]
