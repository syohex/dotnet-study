let totalMoney (n: int) : int =
    let rec totalMoney' n start acc =
        if n <= 0 then
            acc
        else
            let m = min 7 n
            let saved = ((start + start + m - 1) * m) / 2
            totalMoney' (n - m) (start + 1) (acc + saved)

    totalMoney' n 1 0

// 10
totalMoney 4

// 37
totalMoney 10

// 96
totalMoney 20
