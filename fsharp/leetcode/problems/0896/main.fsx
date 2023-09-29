let isMonotonic (nums: int list) : bool =
    match nums with
    | [] -> failwith "never reach here"
    | _ :: [] -> true
    | _ ->
        let cmps =
            nums
            |> List.windowed 2
            |> List.map (fun a -> compare (List.head a) (List.tail a |> List.head))

        List.forall (fun c -> c >= 0) cmps || List.forall (fun c -> c <= 0) cmps

// true
isMonotonic [ 1; 2; 2; 3 ]

// true
isMonotonic [ 6; 5; 4; 4 ]

// false
isMonotonic [ 1; 3; 2 ]

// true
isMonotonic [ 1; 1; 0 ]
