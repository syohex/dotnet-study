let maxAdjacentDistance (nums: int list) : int =
    let rec f nums prev acc =
        match nums with
        | [] -> acc
        | h :: t ->
            let acc = max acc (abs (prev - h))
            f t h acc

    let last = List.last nums
    f nums last 0

// 3
maxAdjacentDistance [ 1; 2; 4 ]

// 5
maxAdjacentDistance [ -5; -10; -5 ]
