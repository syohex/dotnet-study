let maximumElementAfterDecrementingAndRearranging (arr: int list) : int =
    let rec f arr prev =
        match arr with
        | [] -> prev
        | h :: t -> if h - prev > 1 then f t (prev + 1) else f t h

    let arr' = List.sort arr
    f (List.tail arr') 1

// 2
maximumElementAfterDecrementingAndRearranging [ 2; 2; 1; 2; 1 ]

// 3
maximumElementAfterDecrementingAndRearranging [ 100; 1; 1000 ]

// 5
maximumElementAfterDecrementingAndRearranging [ 1; 2; 3; 4; 5 ]
