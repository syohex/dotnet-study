let change (amount: int) (coins: int list) : int =
    let dp = Array.zeroCreate (amount + 1)
    dp.[0] <- 1

    for coin in coins do
        for i in coin..amount do
            dp.[i] <- dp.[i] + dp.[i - coin]

    dp.[amount]

// 4
change 5 [ 1; 2; 5 ]

// 0
change 3 [ 2 ]

// 1
change 10 [ 10 ]
