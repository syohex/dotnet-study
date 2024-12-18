let finalPrices (prices: int list) : int list =
    let rec finalPrices' prices acc =
        match prices with
        | [] -> List.rev acc
        | h :: t ->
            match List.tryFind ((>=) h) t with
            | Some(v) -> finalPrices' t ((h - v) :: acc)
            | None -> finalPrices' t (h :: acc)

    finalPrices' prices []

// [4,2,4,2,3]
finalPrices [ 8; 4; 6; 2; 3 ]

// [1;2;3;4;5]
finalPrices [ 1; 2; 3; 4; 5 ]

// [9,0,1,6]
finalPrices [ 10; 1; 1; 6 ]
