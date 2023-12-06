let totalMoney (n: int) : int =
    let rec totalMoney' i n acc =
        if i >= n then
            acc
        else
            let acc' = acc + (i % 7 + 1) + (i / 7)
            totalMoney' (i + 1) n acc'

    totalMoney' 0 n 0

// 10
totalMoney 4

// 37
totalMoney 10

// 96
totalMoney 20
