let canMakeArithmeticProgression (arr: int list) : bool =
    let diffs =
        arr
        |> List.sort
        |> List.windowed 2
        |> List.map (fun v ->
            match v with
            | a1 :: a2 :: [] -> a2 - a1
            | _ -> failwith "never reach here")

    let diff = List.head diffs
    List.forall (fun a -> a = diff) diffs

// true
canMakeArithmeticProgression [ 3; 5; 1 ]

// false
canMakeArithmeticProgression [ 1; 2; 4 ]
