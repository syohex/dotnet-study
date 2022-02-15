// problem 21
let rec insertAt x n xs =
    match xs with
    | [] -> [ x ]
    | y :: ys ->
        if n = 1 then
            y :: x :: ys
        else
            y :: (insertAt x (n - 1) ys)


// ["a"; "alfa"; "b"; "c"; "d"]
insertAt "alfa" 1 [ "a"; "b"; "c"; "d" ]

// ["a"; "b"; "c"; "alfa"; "d"]
insertAt "alfa" 3 [ "a"; "b"; "c"; "d" ]

// ["a"; "b"; "c"; "d"; "alfa"]
insertAt "alfa" 4 [ "a"; "b"; "c"; "d" ]

// ["a"; "b"; "c"; "d"; "x"]
insertAt "x" 100 [ "a"; "b"; "c"; "d" ]
