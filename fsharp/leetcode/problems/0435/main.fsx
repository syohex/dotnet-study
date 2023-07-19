let eraseOverlapIntervals (intervals: (int * int) list) : int =
    let rec eraseOverlapIntervals' intervals border acc =
        match intervals with
        | [] -> acc
        | (startTime, endTime) :: t ->
            if startTime >= border then
                eraseOverlapIntervals' t endTime acc
            else
                eraseOverlapIntervals' t border (acc + 1)

    let intervals' = List.sortBy snd intervals
    eraseOverlapIntervals' intervals' System.Int32.MinValue 0

// 1
eraseOverlapIntervals [ (1, 2); (2, 3); (3, 4); (1, 3) ]

// 2
eraseOverlapIntervals [ (1, 2); (1, 2); (1, 2) ]

// 0
eraseOverlapIntervals [ (1, 2); (2, 3) ]
