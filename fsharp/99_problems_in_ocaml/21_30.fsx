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

// problem 22
let range m n =
    let rec range' m n acc =
        if m > n then
            acc |> List.rev
        else
            range' (m + 1) n (m :: acc)

    if m < n then
        range' m n []
    else
        range' n m [] |> List.rev

// [4; 5; 6; 7; 8; 9]
range 4 9

// [9; 8; 7; 6; 5; 4]
range 9 4
