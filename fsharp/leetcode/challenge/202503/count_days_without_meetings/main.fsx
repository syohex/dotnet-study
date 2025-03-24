let countDays (days: int) (meetings: (int * int) list) : int =
    let rec countDays' meetings endDay acc =
        match meetings with
        | [] -> acc + days - endDay
        | (s, e) :: t ->
            let acc = if s > endDay then acc + s - endDay - 1 else acc
            let endDay = max endDay e
            countDays' t endDay acc

    countDays' (List.sort meetings) 0 0

// 2
countDays 10 [ (5, 7); (1, 3); (9, 10) ]

// 1
countDays 5 [ (2, 4); (1, 3) ]

// 0
countDays 6 [ (1, 6) ]

// 1
countDays 8 [ (3, 4); (4, 8); (2, 5); (3, 8) ]

//  1
countDays
    14
    [ (6, 11)
      (7, 13)
      (8, 9)
      (5, 8)
      (3, 13)
      (11, 13)
      (1, 3)
      (5, 10)
      (8, 13)
      (3, 9) ]
