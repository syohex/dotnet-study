let canPlaceFlowers (floweredbed: int list) (n: int) : bool =
    let v = (0 :: floweredbed) @ [ 0 ]

    let flowers =
        v
        |> List.windowed 3
        |> List.fold
            (fun acc w ->
                match w with
                | a :: b :: c :: [] ->
                    if b = 0 then
                        if a = 0 && c = 0 then acc + 1 else acc
                    else
                        acc
                | _ -> failwith "never reach here")
            0

    flowers >= n

// true
canPlaceFlowers [ 1; 0; 0; 0; 1 ] 1
// false
canPlaceFlowers [ 1; 0; 0; 0; 1 ] 2

// true
canPlaceFlowers [ 0 ] 1

// true
canPlaceFlowers [ 1 ] 0
