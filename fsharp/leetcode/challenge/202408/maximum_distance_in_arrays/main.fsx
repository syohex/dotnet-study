let maxDistance (arrays: int list list) : int =
    let rec maxDistance' arrays minVal maxVal ret =
        match arrays with
        | [] -> ret
        | h :: t ->
            let front, back = List.head h, List.last h
            let ret = max ret (max (abs (maxVal - front)) (abs (back - minVal)))
            maxDistance' t (min minVal front) (max maxVal back) ret

    match arrays with
    | [] -> failwith "never reach here"
    | h :: t -> maxDistance' t (List.head h) (List.last h) 0

// 4
maxDistance [ [ 1; 2; 3 ]; [ 4; 5 ]; [ 1; 2; 3 ] ]

// 0
maxDistance [ [ 1 ]; [ 1 ] ]

// 3
maxDistance [ [ 1; 5 ]; [ 3; 4 ] ]
