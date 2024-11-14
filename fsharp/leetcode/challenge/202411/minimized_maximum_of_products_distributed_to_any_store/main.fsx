let minimizedMaximum (n: int) (quantities: int list) : int =
    let rec canDistribute count i n products quantities =
        if i >= n then
            false
        else if products <= count then
            match quantities with
            | [] -> true
            | h :: t -> canDistribute count (i + 1) n h t
        else
            canDistribute count (i + 1) n (products - count) quantities

    let rec minimizedMaximum' left right =
        if left >= right then
            left
        else
            let mid = left + (right - left) / 2

            if canDistribute mid 0 n (List.head quantities) (List.tail quantities) then
                minimizedMaximum' left mid
            else
                minimizedMaximum' (mid + 1) right

    minimizedMaximum' 1 (List.max quantities)

// 3
minimizedMaximum 6 [ 11; 6 ]

// 5
minimizedMaximum 7 [ 15; 10; 10 ]
