let isArraySpecial (nums: int list) : bool =
    match nums with
    | [] -> failwith "never reach here"
    | _ :: [] -> true
    | _ ->
        nums
        |> List.windowed 2
        |> List.forall (fun v ->
            let n1, n2 = List.item 0 v, List.item 1 v
            n1 % 2 <> n2 % 2)

// true
isArraySpecial [ 1 ]

// true
isArraySpecial [ 2; 1; 4 ]

// false
isArraySpecial [ 4; 3; 1; 6 ]
