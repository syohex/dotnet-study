let numOfUnplacedFruits (fruits: int list) (baskets: int list) : int =
    let baskets = List.toArray baskets

    fruits
    |> List.fold
        (fun acc fruit ->
            match Array.tryFindIndex (fun m -> m <> -1 && m >= fruit) baskets with
            | Some(v) ->
                baskets.[v] <- -1
                acc
            | None -> acc + 1)
        0

// 1
numOfUnplacedFruits [ 4; 2; 5 ] [ 3; 5; 4 ]

// 0
numOfUnplacedFruits [ 3; 6; 1 ] [ 6; 4; 7 ]
