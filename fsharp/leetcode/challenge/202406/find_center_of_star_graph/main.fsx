let findCenter (edge: (int * int) list) : int =
    match edge with
    | []
    | _ :: [] -> failwith "never reach here"
    | (s1, e1) :: (s2, e2) :: _ -> if s1 = s2 || s1 = e2 then s1 else e1

// 2
findCenter [ (1, 2); (2, 3); (4, 2) ]

// 1
findCenter [ (1, 2); (5, 1); (1, 3); (1, 4) ]
