let removeCoveredIntervals (intervals: (int * int) list) : int =
    let rec removeCoveredIntervals' intervals prev count =
        match intervals with
        | [] -> count
        | (_, e) :: xs ->
            if prev < e then
                removeCoveredIntervals' xs e (count + 1)
            else
                removeCoveredIntervals' xs prev count

    let sorted =
        intervals
        |> List.sortWith (fun (s1, e1) (s2, e2) ->
            if s1 = s2 then
                compare e2 e1
            else
                compare s1 s2)

    removeCoveredIntervals' sorted -1 0

// 2
removeCoveredIntervals [ (1, 4)
                         (3, 6)
                         (2, 8) ]

// 1
removeCoveredIntervals [ (1, 4)
                         (2, 3) ]

// 1
removeCoveredIntervals [ (1, 2)
                         (1, 4)
                         (3, 4) ]
