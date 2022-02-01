let maxProfit (prices: int list) : int =
    let rec maxProfit' prices min max =
        match prices with
        | [] -> max
        | p :: ps ->
            let newMin = if p < min then p else min
            let profit = p - newMin
            let newMax = if profit > max then profit else max
            maxProfit' ps newMin newMax

    maxProfit' prices System.Int32.MaxValue 0

// 5
maxProfit [ 7; 1; 5; 3; 6; 4 ]

// 0
maxProfit [ 7; 6; 4; 3; 1 ]
