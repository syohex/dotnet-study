let productExceptSelf (nums: int list) : int list =
    let leftAcc =
        nums
        |> List.fold
            (fun (acc, prev) num ->
                let p = num * prev
                p :: acc, p)
            ([ 1 ], 1)
        |> fst
        |> List.tail
        |> List.rev

    let rightAcc =
        nums
        |> List.rev
        |> List.fold
            (fun (acc, prev) num ->
                let p = num * prev
                p :: acc, p)
            ([ 1 ], 1)
        |> fst
        |> List.tail

    List.zip leftAcc rightAcc |> List.map (fun (a, b) -> a * b)


// [24,12,8,6]
productExceptSelf [ 1; 2; 3; 4 ]

// [0;0;9;0;0]
productExceptSelf [ -1; 1; 0; -3; 3 ]
