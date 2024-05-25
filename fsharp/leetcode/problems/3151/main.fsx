let isArraySpecial (nums: int list) : bool =
    match nums with
    | []
    | _ :: [] -> true
    | _ ->
        nums
        |> List.windowed 2
        |> List.map (fun v -> (List.head v) &&& 1 <> ((v |> List.tail |> List.head) &&& 1))
        |> List.forall id

// true
isArraySpecial [ 1 ]

// true
isArraySpecial [ 2; 1; 4 ]

// false
isArraySpecial [ 2; 1; 3; 4 ]
