let tupleSameProduct (nums: int list) : int =
    let rec productHash nums acc =
        match nums with
        | [] -> acc
        | h :: t ->
            let acc =
                t
                |> List.fold
                    (fun acc n ->
                        let v = Map.tryFind (h * n) acc |> Option.defaultValue 0
                        Map.add (h * n) (v + 1) acc)
                    acc

            productHash t acc

    productHash nums Map.empty
    |> Map.fold
        (fun acc _ v ->
            if v = 1 then
                acc
            else
                let combinations = (v * (v - 1)) / 2
                acc + combinations * 8)
        0

// 8
tupleSameProduct [ 2; 3; 4; 6 ]

// 16
tupleSameProduct [ 1; 2; 4; 5; 10 ]
