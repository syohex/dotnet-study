open System

let maxProfit (prices: int list) (fee: int) : int =
    let rec maxProfit' prices fee (deposit: int) spent =
        match prices with
        | [] -> deposit
        | h :: t ->
            let deposit' = Math.Max(deposit, spent + h - fee)
            let spent' = Math.Max(spent, deposit' - h)
            maxProfit' t fee deposit' spent'

    maxProfit' (List.tail prices) fee 0 (-1 * List.head prices)

// 8
maxProfit [ 1; 3; 2; 8; 4; 9 ] 2

// 6
maxProfit [ 1; 3; 7; 5; 10; 3 ] 3
