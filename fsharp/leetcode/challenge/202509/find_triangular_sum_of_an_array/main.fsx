let triangleSum (nums: int list) : int =
    let rec triangleSum' nums =
        match nums with
        | [] -> failwith "never reach here"
        | h :: [] -> h
        | _ ->
            let nums = nums |> List.windowed 2 |> List.map (fun v -> (List.sum v) % 10)
            triangleSum' nums

    triangleSum' nums

// 8
triangleSum [ 1; 2; 3; 4; 5 ]

// 5
triangleSum [ 5 ]
