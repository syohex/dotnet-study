let maxCoins (piles: int list) : int =
    let rec maxCoins' tails acc =
        match tails with
        | [] -> acc
        | _ :: h :: t -> maxCoins' t (acc + h)
        | _ -> failwith "never reach here"

    let piles' = List.sort piles
    let len = List.length piles'
    let tails = List.skip (len / 3) piles' |> List.rev
    maxCoins' tails 0

// 9
maxCoins [ 2; 4; 1; 2; 7; 8 ]

// 4
maxCoins [ 2; 4; 5 ]

// 18
maxCoins [ 9; 8; 7; 6; 5; 1; 2; 3; 4 ]
