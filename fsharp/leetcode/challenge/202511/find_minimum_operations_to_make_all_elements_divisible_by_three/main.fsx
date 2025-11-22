let minimumOpeartions (nums: int list) : int =
    nums
    |> List.fold
        (fun acc n ->
            match n % 3 with
            | 0 -> acc
            | 1
            | 2 -> acc + 1
            | _ -> failwith "never reach here")
        0

// 3
minimumOpeartions [ 1; 2; 3; 4 ]

// 0
minimumOpeartions [ 3; 6; 9 ]
