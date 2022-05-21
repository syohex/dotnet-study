open System

let coinChange (coins: int list) (amount: int) : int =
    let dp = Array.init (amount + 1) (fun _ -> amount + 1)
    dp.[0] <- 0

    for i in 1..amount do
        for coin in coins do
            if i >= coin then
                dp.[i] <- Math.Min(dp.[i], dp.[i - coin] + 1)

    if dp.[amount] = amount + 1 then
        -1
    else
        dp.[amount]

// 3
coinChange [ 1; 2; 5 ] 11

// -1
coinChange [ 2 ] 3

// 0
coinChange [ 1 ] 0

// 20
coinChange [ 186; 419; 83; 408 ] 6249
