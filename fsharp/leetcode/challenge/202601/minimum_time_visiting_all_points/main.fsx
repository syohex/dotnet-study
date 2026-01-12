let minTimeToVisitAllPoints (points: (int * int) list) : int =
    let rec f points (prevX, prevY) acc =
        match points with
        | [] -> acc
        | (x, y) :: t ->
            let acc = acc + max (abs (x - prevX)) (abs (y - prevY))
            f t (x, y) acc

    f (List.tail points) (List.head points) 0

// 7
minTimeToVisitAllPoints [ (1, 1); (3, 4); (-1, 0) ]

// 5
minTimeToVisitAllPoints [ (3, 2); (-2, 2) ]
