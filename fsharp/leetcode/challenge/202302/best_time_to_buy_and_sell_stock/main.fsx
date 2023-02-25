open System

let maxProfit (prices: int list) : int =
    let rec maxProfit' (prices: int list) (min: int) (ret: int) =
        match prices with
        | [] -> ret
        | h :: t -> maxProfit' t (Math.Min(min, h)) (Math.Max(ret, h - min))

    maxProfit' prices Int32.MaxValue 0

// 5
maxProfit [ 7; 1; 5; 3; 6; 4 ]

// 0
maxProfit [ 7; 6; 4; 3; 1 ]
